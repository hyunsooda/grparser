package mypack

import (
	"fmt"
	"log"
	"net/http"
	"unsafe"
)

type status uint8

const (
	good status = iota
	notbad
)

var ASD uint8 = 22

func measure(g geometry) {
	fmt.Println(g)
	fmt.Println(g.area())
	fmt.Println(g.perim())
}

type myasd struct {
	ABC
}

type geometry interface {
	area() float64
	perim() float64
}

type rect struct {
	width, height float64
}

func (r rect) area() float64 {
	return r.width * r.height
}

func (r rect) perim() float64 {
	return 2*r.width + 2*r.height
}

type InterfaceExample interface {
	Hi() string
}

type ABC struct{}

func (abc ABC) Hi() string {
	return "hello"
}

func (abc *ABC) Hi2() string {
	return "hello"
}

func myFunc(a int, b string) (float32, float64) {
	return 3.14, 3.14
}

func GetSum() int {
	sum := 0
	for i := 0; i < 5; i++ {
		sum += i
	}
	return sum
}

func types() {
	a := int8(1) + int8(2)
	b := int16(1) + int16(2)
	c := int32(1) + int32(2)
	d := int64(1) + int64(2)

	aa := uint(1) + uint(2)
	bb := uint8(1) + uint8(2)
	cc := uint16(1) + uint16(2)
	dd := uint32(1) + uint32(2)
	ee := uint64(1) + uint64(2)

	_b, r := byte(123), rune('한')

	byteArr := []byte("this is byte array")
	runeArr := []rune("한글")
	_ = uint(a)

	ca := 1 + 2i
	cb := complex(1, 2)
	ccc := 2i - 1
	fmt.Println(imag(ca), real(ca))
	fmt.Println(imag(cb), real(cb))
	_ = ccc

	abc := ABC{}
	interfaceABC := InterfaceExample(abc)
	interfaceABC.Hi()

	v := int(123)
	vptr := uintptr(v)
	vptrUnsafe := unsafe.Pointer(vptr)
	vptr2 := (*int)(vptrUnsafe)
	_ = *vptr2
	unsafeNil := unsafe.Pointer(nil)
	_ = unsafeNil

	var var1 uintptr = 0xc82000c290
	_ = var1

	m := make(map[string]string)
	m["hello"] = "world"
	m["world"] = "hello"
	for k, v := range m {
		_ = v
		delete(m, k)
	}

	funcPtr1 := myFunc
	_, _ = funcPtr1(1, "hello")

	arr := make([]int, 5)
	_ = arr

	_, _, _, _ = a, b, c, d
	_, _, _, _, _ = aa, bb, cc, dd, ee
	_, _ = _b, r
	_, _ = byteArr, runeArr

	resp, err := http.Get("https://myhompage.com/sub/123")
	if err != nil {
		log.Fatalln(err, resp)
	}
	a123 := "wwwwwwwwwwweeeeeeeeeeeeeeeeaaaaaaaaaa"
	_ = a123

	go func() {
		a := "hello"
		_ = a
	}()
	go func() {
		a := "hello"
		_ = a
	}()

	go myFunc(1, "hello")

	ch1 := make(chan bool)
	ch2 := make(chan bool)
	ch1 <- true
	select {
	case value := <-ch1:
		_ = value
	case value := <-ch2:
		_ = value
	default:
		a := "hello"
		_ = a

		/*
			case ch1 <- true:
				_ = ch1
			case <-ch2:
				_ = ch2
		*/
	}

	rrr := rect{width: 3, height: 4}
	measure(rrr)

	const (
		good status = iota
		notbad
	)

	fmt.Println(good + notbad + notbad)
	asd := good
	_ = asd

	var db [][]byte
	_ = db

	d1 := make(chan int)
	d2 := (<-chan int)(d1)
	_ = d2

	sm := make(map[string]string)
	vm, _ := sm["hello"]
	_ = vm

	idx1, idx2, idx3 := 0, 0, 0
	go func() int {
		_ = idx1 + 1
		_ = idx2 + 1
		_ = idx3 + 1
		return idx1 + idx2 + idx3
	}()
	sl := []int{1, 2, 3, 4, 5}
	slPtr := (*[5]int)(sl)
	_ = slPtr

	aa1, bb1 := 1, 2
	cc1 := aa1 + bb1
	_ = cc1

	ch123 := make(chan int)
	asd123, qwe := <-ch123
	ch123 <- 123
	_, _ = asd123, qwe
	panic("hello")
}
