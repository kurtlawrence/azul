
use std::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
	cell::RefCell,
	rc::{Rc, Weak},
	any::Any,
	ops::{Deref, DerefMut},
};

pub struct AppValue<T> {
	inner: Rc<RefCell<T>>,
}

impl<T> AppValue<T> {
	pub fn new(value: T) -> Self {
		Self { inner: Rc::new(RefCell::new(value)) }
	}
}

impl<T> Deref for AppValue<T> {
	type Target = T;

	fn deref(&self) -> &T {
		let brw = self.inner.borrow();
		let typed: &T = &brw;
		let ptr: *const T = typed as *const T;
		let ret: &T = unsafe { ptr.as_ref().expect("should not be null") };

		ret
	}
}

impl<T> DerefMut for AppValue<T> {
	fn deref_mut(&mut self) -> &mut T {
		let mut brw = self.inner.borrow_mut();
		let typed: &mut T = &mut brw;
		let ptr: *mut T = typed as *mut T;
		let ret: &mut T = unsafe { ptr.as_mut().expect("should not be null") };

		ret
	}
}

/// A `StackCheckedPointer<T>` is a type-erased, raw pointer to a
/// value **inside** of `T`.
///
/// Since we know that the pointer is "checked" to be contained (on the stack)
/// within `&T as usize` and `&T as usize + mem::size_of::<T>()`,
/// `StackCheckedPointer<T>` has the same lifetime as `T`
/// (but the type is erased, so it can be stored independent from `T`s lifetime).
///
/// Note for enums: Should the pointer point to an enum instead of a struct and
/// the enum (which in Rust is a union) changes its variant, the behaviour of
/// invoking this pointer is undefined (likely to segfault).
pub struct StackCheckedPointer<T> {
    /// Type-erased pointer to a value on the stack in the `app_data.data`
    /// model. When invoking default methods, we have to store a pointer to
    /// the data we should update, but storing it in a `Box<T>` to
    /// erase the type doesn't help anything - we trust the user of this
    /// pointer to know the exact type of this pointer.
    internal: Weak<Any>,
    /// Marker so that one stack checked pointer can't be shared across
    /// two data models that are both `T: Layout`.
    marker: PhantomData<T>,
	/// Used for hashing and equality.
	ptr: usize,
}

impl<T> StackCheckedPointer<T> {

    /// Validates that the pointer to U is contained in T.
    ///
    /// This means that the lifetime of U is the same lifetime as T -
    /// the returned `StackCheckedPointer` is valid for as long as `stack`
    /// is valid.
    pub fn new<U: Sized + 'static>(pointer: &AppValue<U>) -> Self {
		let ptr: usize = Rc::into_raw(Rc::clone(&pointer.inner)) as usize;

		let weak = Rc::downgrade(&pointer.inner);

		Self {
			internal: weak,
			marker: PhantomData,
			ptr,
		}
    }

    #[inline]
    pub(crate) fn cast<'a, U: Sized + 'static>(&'a self) -> &'a mut U {
       
		let rc = self.internal.upgrade()
			.and_then(|rc| rc.downcast::<RefCell<U>>().ok()).expect("failed casting to RefCell");

		let mut refmut = rc.try_borrow_mut().ok().expect("failed borrowing mutably");

		let typed: &mut U = &mut refmut;
		let ptr: *mut U = typed as *mut U;
		let ret: &mut U = unsafe { ptr.as_mut().expect("should not be null") };

		ret
    }
}

// #[derive(Debug, Clone, PartialEq, Hash, Eq)] for StackCheckedPointer<T>

impl<T> fmt::Debug for StackCheckedPointer<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
            "StackCheckedPointer {{ internal: 0x{:x}, marker: {:?} }}",
            self.ptr, self.marker
        )
    }
}

impl<T> Clone for StackCheckedPointer<T> {
    fn clone(&self) -> Self {
        StackCheckedPointer { 
			internal: self.internal.clone(), 
			marker: self.marker.clone(), 
			ptr: self.ptr
		}
    }
}

impl<T> Hash for StackCheckedPointer<T> {
  fn hash<H>(&self, state: &mut H) where H: Hasher {
    state.write_usize(self.ptr);
  }
}

impl<T> PartialEq for StackCheckedPointer<T> {
  fn eq(&self, rhs: &Self) -> bool {
    self.ptr == rhs.ptr
  }
}

impl<T> Eq for StackCheckedPointer<T> { }
// impl<T> Copy for StackCheckedPointer<T> { }



/// Returns true if U is a type inside of T
///
/// i.e:
///
/// ```ignore
/// # struct Data { i: usize, p: Vec<usize> }
/// let data = Data { i: 5, p: vec![5] };
///
/// // true because i is inside of data
/// assert_eq!(is_subtype_of(&data, &data.i), true);
/// // true because p is inside of data
/// assert_eq!(is_subtype_of(&data, &data.p), true);
/// // false because p is heap-allocated, therefore not inside of data
/// assert_eq!(is_subtype_of(&data, &data.p[0]), false);
/// ```
fn is_subtype_of<T, U>(data: &T, subtype: &U) -> bool {

    // determine in which direction the stack grows
    use std::mem::size_of;

    struct Invalid {
        a: u64,
        b: u64,
    }

    let invalid = Invalid { a: 0, b: 0 };

    let stack_grows_down = &invalid.b as *const _ as usize > &invalid.a as *const _ as usize;

    // calculate if U is a subtype of T
    let st = subtype as *const _ as usize;
    let t = data as *const _ as usize;

    if stack_grows_down {
        st >= t && st + size_of::<U>() <= t + size_of::<T>()
    } else {
        st <= t && st - size_of::<U>() >= t - size_of::<T>()
    }
}

#[test]
fn test_reflection_subtyping() {

    struct Data { i: usize, p: Vec<usize> }
    let data = Data { i: 5, p: vec![5] };

    assert_eq!(is_subtype_of(&data, &data.i), true);
    assert_eq!(is_subtype_of(&data, &data.p), true);
    assert_eq!(is_subtype_of(&data, &data.p[0]), false);
}

#[test]
fn deref_same_ptr() {
	let mut value = AppValue {
		inner: Rc::new(RefCell::new(String::from("Hello, world!")))
	};

	{
		let ptr = &*value as *const String as usize;
		assert_eq!(ptr, value.inner.as_ptr() as usize);
		let brw = value.inner.borrow();
		assert_eq!(ptr, &*brw as *const String as usize);
	}


	let ptr = &mut *value as *mut String as usize;
	assert_eq!(ptr, value.inner.as_ptr() as usize);
}