/*
** gc.h
**
** Made by Edison Mera
** Login   <edison@vaioedison>

This software was excerpted from chapter two of The Art of C++,
written by Herbert Schildt (McGraw-Hill/Osborne, 2004; ISBN:
0072255129).

**
** Started on  Sun Mar 25 16:23:10 2007 Edison Mera
** Last update Fri Mar 30 00:09:43 2007 Edison Mera
*/

#ifndef 	_gc_H_
# define	_gc_H_


// A single-threaded garbage collector.
# include <iostream>
# include <typeinfo>
# include <cstdlib>

# include "Settings.h"
# include "glue_map.h"

using namespace wam;
using namespace std;

// Exception thrown when an attempt is made to
// use an Iter that exceeds the range of the
// underlying object.
//
class OutOfRangeExc {
  // Add functionality if needed by your application.
};
// An iterator-like class for cycling through arrays
// that are pointed to by GCPtrs. Iter pointers
// ** do not ** participate in or affect garbage
// collection. Thus, an Iter pointing to
// some object does not prevent that object
// from being recycled.
//

template <typename T> class Iter {
  T *ptr;   // current pointer value
  T *end;   // points to element one past end
  T *begin; // points to start of allocated array
  unsigned length; // length of sequence
 public:
  Iter() {
    ptr = end = begin = NULL;
    length = 0;
  }
  Iter(T *p, T *first, T *last) {
    ptr = p;
    end = last;
    begin = first;
    length = last - first;
  }
  // Return length of sequence to which this
  // Iter points.
  unsigned size() { return length; }
  // Return value pointed to by ptr.
  // Do not allow out-of-bounds access.
  T &operator*() {
    if( (ptr >= end) || (ptr < begin) )
      throw OutOfRangeExc();
    return *ptr;
  }
  // Return address contained in ptr.
  // Do not allow out-of-bounds access.
  T *operator->() {
    if( (ptr >= end) || (ptr < begin) )
      throw OutOfRangeExc();
    return ptr;
  }
  // Prefix ++.
  Iter operator++() {
    ptr++;
    return *this;
  }
  // Prefix --.
  Iter operator--() {
    ptr--;
    return *this;
  }
  // Postfix ++.
  Iter operator++(int notused) {
    T *tmp = ptr;
    ptr++;
    return Iter<T>(tmp, begin, end);
  }
  // Postfix --.
  Iter operator--(int notused) {
    T *tmp = ptr;
    ptr--;
    return Iter<T>(tmp, begin, end);
  }
  // Return a reference to the object at the
  // specified index. Do not allow out-of-bounds
  // access.
  T &operator[](int i) {
    if( (i < 0) || (i >= (end-begin)) )
      throw OutOfRangeExc();
    return ptr[i];
  }
  // Define the relational operators.
  bool operator==(Iter op2) {
    return ptr == op2.ptr;
  }
  bool operator!=(Iter op2) {
    return ptr != op2.ptr;
  }
  bool operator<(Iter op2) {
    return ptr < op2.ptr;
  }
  bool operator<=(Iter op2) {
    return ptr <= op2.ptr;
  }
  bool operator>(Iter op2) {
    return ptr > op2.ptr;
  }
  bool operator>=(Iter op2) {
    return ptr >= op2.ptr;
  }
  // Subtract an integer from an Iter.
  Iter operator-(int n) {
    ptr -= n;
    return *this;
  }
  // Add an integer to an Iter.
  Iter operator+(int n) {
    ptr += n;
    return *this;
  }
  // Return number of elements between two Iters.
  int operator-(Iter<T> &itr2) {
    return ptr - itr2.ptr;
  }
};
// This class defines an element that is stored
// in the garbage collection information list.
//
template <typename T> class GCInfo {
 public:
  unsigned refcount; // current reference count
  T *memPtr; // pointer to allocated memory
  /* isArray is true if memPtr points
     to an allocated array. It is false
     otherwise. */
  bool isArray() const { return arraySize != 0; }; // true if pointing to array
  /* If memPtr is pointing to an allocated
     array, then arraySize contains its size */
  unsigned arraySize; // size of array
  // Here, mPtr points to the allocated memory.
  // If this is an array, then size specifies
  // the size of the array.
 GCInfo(T * const mPtr, unsigned size=0)
   {
     refcount = 1;
     memPtr = mPtr;
     arraySize = size;
   }
};

// Overloading operator== allows GCInfos to be compared.
// This is needed by the STL list class.
template <typename T> bool operator==(const GCInfo<T> &ob1,
				      const GCInfo<T> &ob2) {
  return (ob1.memPtr == ob2.memPtr);
}
// GCPtr implements a pointer type that uses
// garbage collection to release unused memory.
// A GCPtr must only be used to point to memory
// that was dynamically allocated using new.
// When used to refer to an allocated array,
// specify the array size.
//

template <typename T, int size=0> class InitGCPtr {
 public:
 InitGCPtr();
};

template <typename T, int size=0> class GCPtr {
  // gclist maintains the garbage collection list.
  
  static glue_map<T *, GCInfo<T> > gclist;
  // addr points to the allocated memory to which
  // this GCPtr pointer currently points.
  T *addr;
  /* isArray is true if this GCPtr points
     to an allocated array. It is false
     otherwise. */
  bool isArray() { return arraySize!=0; }; // true if pointing to array
  // If this GCPtr is pointing to an allocated
  // array, then arraySize contains its size.
  unsigned arraySize; // size of the array
  // Return an iterator to pointer info in gclist.
  typename glue_map<T *, GCInfo<T> >::iterator findPtrInfo(T * ptr) {
    return gclist.find(ptr);
  };
  
 public:
  // Define an iterator type for GCPtr<T>.
  typedef Iter<T> GCiterator;

  // force initialization of this class
  static InitGCPtr<T, size> initGCPtr;

  // Construct both initialized and uninitialized objects.
  GCPtr(T * const t=NULL)
  {
    typename glue_map<T *, GCInfo<T> >::iterator p = findPtrInfo(t);
    // If t is already in gclist, then
    // increment its reference count.
    // Otherwise, add it to the list.
    if(p != gclist.end())
      p->second.refcount++; // increment ref count
    else {
      // Create and store this entry.
      GCInfo<T> gcObj(t, size);
      gclist.insert(make_pair(t, gcObj));
    }
    addr = const_cast<T *>(t);
    arraySize = size;
# ifdef GC_DISPLAY
    cout << "Constructing GCPtr<" << T::getClassName() << ", " << size << ">"
	 << "(" << (void *)t << ")";
    if(isArray())
      cout << " Size is " << arraySize << endl;
    else
      cout << endl;
# endif
  }
  // Copy constructor.
  GCPtr(const GCPtr &ob) {
    typename glue_map<T *, GCInfo<T> >::iterator p =
    findPtrInfo(ob.addr);
    p->second.refcount++; // increment ref count
    addr = ob.addr;
    arraySize = ob.arraySize;
# ifdef GC_DISPLAY
    cout << "Constructing copy.";
    if(isArray())
      cout << " Size is " << arraySize << endl;
    else
      cout << endl;
# endif
  }
  // Destructor for GCPtr.
  ~GCPtr();
  // Collect garbage. Returns true if at least
  // one object was freed.
  static bool collect();
  // Overload assignment of pointer to GCPtr.
  T *operator=(T *t);
  // Overload assignment of GCPtr to GCPtr.
  const GCPtr &operator=(const GCPtr &rv);
  // Return a reference to the object pointed
  // to by this GCPtr.
  T &operator *() const {
    return *addr;
  }
  // Return the address being pointed to.
  T *operator->() { return addr; }
  T *operator->() const { return addr; }
  // Return a reference to the object at the
  // index specified by i.
  T &operator[](int i) {
    return addr[i];
  }
  bool operator==(const GCPtr &op2) const {
    return addr == op2.addr;
  }
  bool operator!=(const GCPtr &op2) const {
    return addr != op2.addr;
  }
  bool operator==(const T * const t) const {
    return addr == t;
  }
  bool operator!=(const T * const t) const {
    return addr != t;
  }
  // Conversion function to T *.
  operator T *() const { return addr; }
  // Return an Iter to the start of the allocated memory.
  Iter<T> begin() {
    int aSize;
    if(isArray()) aSize = arraySize;
    else aSize = 1;
    return Iter<T>(addr, addr, addr + aSize);
  }
  // Return an Iter to one past the end of an allocated array.
  Iter<T> end() {
    int aSize;
    if(isArray()) aSize = arraySize;
    else aSize = 1;
    return Iter<T>(addr + aSize, addr, addr + aSize);
  }
  // Return the size of gclist for this type
  // of GCPtr.
  static void justFree(T *t) {
    t->~T();
    ::operator delete(t, GCInfo<T>::stack);
  };

  static void freePtr(typename glue_map<T *, GCInfo<T> >::iterator &p);
  static void displayPtr(const GCInfo<T> &p);
  static bool collectPtr(typename glue_map<T *, GCInfo<T> >::iterator &p) {
    if (p->second.refcount==0) {
      freePtr(p);
      // cerr << "Smart delete" << endl;
      return true;
    }
    else
      return false;
  };
  static size_t gclistSize() { return gclist.size(); }
  // A utility function that displays gclist.
  static void showList();
  // Clear gclist when program exits.
  static void shutdown();
  static const char *getCollectedClassName();
};

template <typename T, int size>
  inline InitGCPtr<T,size>::InitGCPtr() {
  cout << "registering shutdown in atexit" << endl;
  atexit(GCPtr<T,size>::shutdown); };

// Creates storage for the static variables
template <typename T, int size>
  glue_map<T *, GCInfo<T> > GCPtr<T, size>::gclist;

template <typename T, int size>
  InitGCPtr<T, size> GCPtr<T, size>::initGCPtr;

// Destructor for GCPtr.
template <typename T, int size>
  GCPtr<T, size>::~GCPtr() {
  typename glue_map<T *, GCInfo<T> >::iterator p = findPtrInfo(addr);
  if(p==gclist.end()) // The pointer was already removed
    return;
  if(p->second.refcount) p->second.refcount--; // decrement ref count
# ifdef GC_DISPLAY
  cout << "GCPtr<" << T::getClassName() << ", " << size
       << "> ";
  displayPtr(p->second);
  cout << " going out of scope. refcount=" << p->second.refcount << "."
       << endl;
# endif
  collectPtr(p); // collect memory if not referenced anymore
  // Collect garbage when a pointer goes out of scope.
  // collect();
  // For real use, you might want to collect
  // unused memory less frequently, such as after
  // gclist has reached a certain size, after a
  // certain number of GCPtrs have gone out of scope,
  // or when memory is low.
}
// Collect garbage.  Returns true if at least
// one object was freed.
template <typename T, int size>
  bool GCPtr<T, size>::collect() {
  bool memfreed = false;
# ifdef GC_DISPLAY
  cout << "Before garbage collection for ";
  showList();
# endif
  typename glue_map<T *, GCInfo<T> >::iterator p;
  p = gclist.begin();
  while(p != gclist.end()) {
    // Scan gclist looking for unreferenced pointers.
    // If in-use, skip.
    if (collectPtr(p))
      memfreed = true;
    p++;
  };
# ifdef GC_DISPLAY
  cout << "After garbage collection for ";
  showList();
# endif
  return memfreed;
};

template <typename T, int size>
  void GCPtr<T, size>::displayPtr(const GCInfo<T> &p) {
  T *memPtr = p.memPtr;
  bool isArray = p.isArray();
  size_t arraySize = p.arraySize;
  if (memPtr) {
    if(isArray) {
      cout << "[ " << endl;
      cout << memPtr[0] << endl;
      for (size_t i=1 ; i < arraySize; i++)
	cout << ", " << memPtr[i];
      cout << "] (" << (void *)(memPtr) << ")[" << arraySize << "]";
    }
    else {
      cout << *(T *) memPtr << " (" <<(void *)(memPtr) << ")";
    }
  }
  else
    cout << "(NULL)";
}

template <typename T, int size>
  void GCPtr<T, size>::freePtr(typename glue_map<T *, GCInfo<T> >::iterator &p)
{
  // Remove unused entry from gclist.
  // Free memory unless the GCPtr is null.
  T *memPtr = p->second.memPtr;
  bool isArray = p->second.isArray();
  size_t arraySize = p->second.arraySize;
# ifdef GC_DISPLAY
  cout << "Deleting: ";
  displayPtr(p->second);
  cout << endl;
# endif
  gclist.erase(p);
  if(memPtr) {
    if(isArray) {
# ifdef DEBUG_STACK
      if (GCInfo<T>::stack) {
	for (size_t i = arraySize - 1; i!=(size_t)(-1); i--)
	  justFree(memPtr + i);
      }
      else
# endif
	delete[] memPtr; // delete array
    }
    else {
# ifdef DEBUG_STACK
      if (GCInfo<T>::stack) {
	justFree(memPtr);
      }
      else
# endif
	delete memPtr; // delete single element
    }
  }
};

// Overload assignment of pointer to GCPtr.
template <typename T, int size>
  T * GCPtr<T, size>::operator=(T *t) {
  typename glue_map<T *, GCInfo<T> >::iterator p = findPtrInfo(addr),
  q = findPtrInfo(t);
  // First, decrement the reference count
  // for the memory currently being pointed to.
  p->second.refcount--;
  // Next, if the new address is already
  // existent in the system, increment its
  // count. Otherwise, create a new entry
  // for gclist.
  if(q != gclist.end()) {
    q->second.refcount++;
  }
  else {
    // Create and store this entry.
    GCInfo<T> gcObj(t, size);
    gclist.insert(make_pair(t, gcObj));
  }
  collectPtr(p);
  addr = t; // store the address.
  return t;
}
// Overload assignment of GCPtr to GCPtr.
  template <typename T, int size>
  const GCPtr<T, size> & GCPtr<T, size>::operator=(const GCPtr &rv) {
    typename glue_map<T *, GCInfo<T> >::iterator p = findPtrInfo(addr),
    q = findPtrInfo(rv.addr);
    T * t = rv.addr; //This is to avoid CodeWard errors in CodeGear C++ Builder
    // First, decrement the reference count
    // for the memory currently being pointed to.
    p->second.refcount--;
    // Next, increment the reference count of
    // the new address.
    q->second.refcount++; // increment ref count
    collectPtr(p);
    addr = t;       // store the address.
    return rv;
  }
  // A utility function that displays gclist.
    template <typename T, int size>
  void GCPtr<T, size>::showList() {
      typename glue_map<T *, GCInfo<T> >::iterator p;
      cout << "gclist<" << T::getClassName() << ", "
	   << size << ">:\n";
      cout << "memPtr\trefcount value\n";
      if(gclist.begin() == gclist.end()) {
	cout << "           -- Empty --\n\n";
	return;
      }
      for(p = gclist.begin(); p != gclist.end(); p++) {
	cout << (void *)p->second.memPtr
	     << "\t" << p->second.refcount << "\t";
	if(p->second.memPtr) cout << *p->second.memPtr;
	else cout << "---";
	cout << endl;
      }
      cout << endl;
    }

// Clear gclist when program exits.
template <typename T, int size>
      void GCPtr<T, size>::shutdown() {
  if(gclistSize() == 0) return; // list is empty
# ifdef GC_DISPLAY
  cout << "Before collecting for shutdown() for "
       << T::getClassName()
       << "\n";
# endif
  typename glue_map<T *, GCInfo<T> >::iterator p = gclist.begin();
  while (p != gclist.end()) {
    cerr << "WARNING: Collecting at shutdown. Class "
	 << T::getClassName()
	 << endl;
    freePtr(p);
    p = gclist.begin();
  }
# ifdef GC_DISPLAY
  cout << "After collecting for shutdown() for "
       << T::getClassName()
       << "\n";
# endif
}

#endif // _gc_H_
