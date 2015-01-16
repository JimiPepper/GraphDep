package fr.inria.lille.graphdep.svg

/**
 * @author Romain Philippon
 */

abstract class ColorSystem

case class RGB(red : Int, green : Int, blue : Int) extends ColorSystem {
  if(red < 0  || red > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (red : "+ red +")")
  }

  if(green < 0  || green > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (green : "+ green +")")
  }

  if(blue < 0  || blue > 255) {
    throw new IllegalArgumentException("RGB values must be between 0 and 255 (blue : "+ blue +")")
  }

  override def toString : String = {
    "rgb("+ red +", "+ green +", "+ blue +")"
  }
}

case class HexaRGB(value : String) extends ColorSystem {
  if(value(0) == '#') {
    throw new IllegalArgumentException("HexaRGB can't start with #")
  }

  override def toString : String = {
    "#"+ value
  }
}
