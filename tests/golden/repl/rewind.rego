package rewind

crashing_rule {
  x = 1
  x + "hi" > "wat"
}

failing_rule {
  x = 1
  x = 2
}
