package Model;

import scala.swing.{AbstractButton, Button, Graphics2D}
import java.awt.{Color, Graphics, RenderingHints}
import java.io.File
import javax.imageio.ImageIO
import scala.xml.NodeSeq.Empty.text

class Btn(var siz:swing.Dimension,var tex:String) extends Button {
    text=tex
    minimumSize = siz
    maximumSize = siz
    preferredSize = siz
    private var over = false
    private var color:Color = null
    private var colorOver:Color = null
    private var colorClick:Color = null
    private var borderColor:Color = null
    private var radius = 0
    this.contentAreaFilled = false

    def isOver: Boolean = over

    def setOver(over: Boolean): Unit = {
        this.over = over
    }

    def getColor: Color = color

    def setColor(color: Color): Unit = {
        this.color = color
        this.background=color
    }

    def getColorOver: Color = colorOver

    def setColorOver(colorOver: Color): Unit = {
        this.colorOver = colorOver
    }

    def getColorClick: Color = colorClick

    def setColorClick(colorClick: Color): Unit = {
        this.colorClick = colorClick
    }

    def getBorderColor: Color = borderColor

    def setBorderColor(borderColor: Color): Unit = {
        this.borderColor = borderColor
    }

    def getRadius: Int = radius

    def setRadius(radius: Int): Unit = {
        this.radius = radius
    }


    override protected def paintComponent(grphcs: Graphics2D): Unit = {
        val g2 = grphcs.asInstanceOf[Graphics2D]
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        //  Paint Border
        g2.setColor(borderColor)
        g2.fillRoundRect(0, 0, siz.width, siz.height, radius, radius)
        g2.setColor(background)
        //  Border set 2 Pix
        g2.fillRoundRect(2, 2, siz.width - 4, siz.height - 4, radius, radius)
        super.paintComponent(grphcs)
    }
}