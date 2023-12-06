#ifndef C_SRC_MACROS_H_
#define C_SRC_MACROS_H_

#define UNUSED(expr) do { (void)(expr); } while (0)
#define BIN_TO_STR(x) reinterpret_cast<const char*>(x)
#define BIN_TO_STRING_VIEW(x) std::string_view(BIN_TO_STR(x.data), x.size)
#define scoped_ptr(Name, Type, New, Free) std::unique_ptr<Type, decltype(&Free)>Name (New, &Free)

#define DISALLOW_ASSIGN(TypeName) void operator=(const TypeName&)
#define DISALLOW_COPY_AND_ASSIGN(TypeName) TypeName(const TypeName&); DISALLOW_ASSIGN(TypeName)
#define DISALLOW_IMPLICIT_CONSTRUCTORS(TypeName) TypeName(); DISALLOW_COPY_AND_ASSIGN(TypeName)

#endif
