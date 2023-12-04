#ifndef C_SRC_MACROS_H_
#define C_SRC_MACROS_H_

#define UNUSED(expr) do { (void)(expr); } while (0)
#define BIN_TO_STR(x) reinterpret_cast<const char*>(x)
#define scoped_ptr(Name, Type, New, Free) std::unique_ptr<Type, decltype(&Free)>Name (New, &Free)

#endif
