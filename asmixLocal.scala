//
//


import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.JOptionPane.showMessageDialog
import swing.ComboBox
import scala.collection.parallel._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.io.Source
import java.awt.Dimension
import java.io.FileNotFoundException
import java.io.IOException
import java.io.File

object ScalaShop {

  class ScalaShopFrame extends JFrame("AsmixLocal") {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try {
        op(p)
      } finally {
        p.close()
      }
    }

    setDefaultCloseOperation(3)
    setSize(640, 480)
    setLayout(new BorderLayout)
    /*
        val rightpanel = new JPanel
        rightpanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))
        rightpanel.setLayout(new BorderLayout)
        add(rightpanel, BorderLayout.EAST)
    */

    val tabbedPane = new JTabbedPane

    val allControls = new JPanel
    val plsControls = new JPanel
    allControls.setLayout(new BoxLayout(allControls, BoxLayout.Y_AXIS))
    plsControls.setLayout(new BoxLayout(plsControls, BoxLayout.Y_AXIS))
    //add(allControls, BorderLayout.NORTH)
    add(tabbedPane, BorderLayout.NORTH)
    tabbedPane.addTab("Options", allControls)
    tabbedPane.addTab("Playlists", plsControls)
    // Volumes count selection

    val volumeLabelsControls = new JPanel
    volumeLabelsControls.setLayout(new GridLayout(0, 13))
    allControls.add(volumeLabelsControls)

    val volumeLabelsText = Array[String]("Master", "Фон Л", "Фон П", "Рек Л", "Рек П")
    for (x <- volumeLabelsText) {
      volumeLabelsControls.add(new JLabel(x))
    }

    val volumeControls = new JPanel
    volumeControls.setLayout(new GridLayout(0, 13))
    allControls.add(volumeControls)

    val filenameMusic: String = "task/MUSIC_VOL"
    val filenameJingl: String = "task/JINGL_VOL"

    def getVol(filename: String, defaultValue: String): String = try {
      Source.fromFile(filename).getLines.mkString
    } catch {
      case e: Exception => defaultValue
    }

    val tmpMusicVol = getVol(filenameMusic, "0:0:0").split(":")
    val musicVol = if (tmpMusicVol.length == 3) tmpMusicVol else Array.fill(3)("0")

    val tmpJinglVol = getVol(filenameMusic, "0:0:0").split(":")
    val jinglVol = if (tmpMusicVol.length == 3) tmpJinglVol else Array.fill(3)("0")

    var vol = Array[JSpinner]()
    val volumes = musicVol :+ jinglVol(1) :+ jinglVol(2)
    for (x <- volumes) {
      vol :+= new JSpinner(new SpinnerNumberModel(x.toInt, 0, 100, 1)); volumeControls.add(vol.last)
    }

    // Equalizers count selection
    val eqControls = new JPanel
    eqControls.setLayout(new GridLayout(0, 10))
    allControls.add(eqControls)

    val filenameEq = "eq"
    val tmpEq = getVol(filenameEq, "0:0:0:0:0:0:0:0:0:0").split(":")
    val eq = if (tmpEq.length == 19) tmpEq else Array.fill(10)("0")

    val eqLabelsText = Array[String]("31.25 Hz", "62.5 Hz", "125 Hz", "250 Hz", "500 Hz", "1 kHz", "2 kHz", "4 kHz", "8 kHz", "16 kHz")
    for (x <- eqLabelsText) {
      eqControls.add(new JLabel(x))
    }

    var equalizer = Array[JSpinner]()
    for (x <- eq) {
      equalizer :+= new JSpinner(new SpinnerNumberModel(x.toInt, -12, 12, 1)); eqControls.add(equalizer.last)
    }

    //  Ping

    val pingLabelsControls = new JPanel
    pingLabelsControls.setLayout(new GridLayout(0, 4))
    allControls.add(pingLabelsControls)

    val filenameRm = ".rm"
    val defaultIdPc = "635"
    val tagId = "ID:"
    val rmLinesIdPc = try {
      Source.fromFile(filenameRm).getLines.toArray filter (_.startsWith(tagId))
    } catch {
      case e: Exception => Array(tagId + defaultIdPc)
    }
    val idPc = rmLinesIdPc.length match {
      case 0 => defaultIdPc
      case _ => rmLinesIdPc(0).split(":")(1)
    }

    pingLabelsControls.add(new JLabel("ID", SwingConstants.RIGHT))
    val idPcJSpinner = new JSpinner(new SpinnerNumberModel(idPc.toInt, 1, 5000, 1))
    pingLabelsControls.add(idPcJSpinner)

    val defaultInterval = "60"
    val tagInterval = "INTERVAL:"
    val rmLinesInterval = try {
      Source.fromFile(filenameRm).getLines.toArray filter (_.startsWith(tagInterval))
    } catch {
      case e: Exception => Array(tagInterval + defaultInterval)
    }
    val interval = rmLinesInterval.length match {
      case 0 => defaultInterval
      case _ => rmLinesInterval(0).split(":")(1)
    }

    pingLabelsControls.add(new JLabel("Ping", SwingConstants.RIGHT))
    val ping = new JSpinner(new SpinnerNumberModel(interval.toInt, 1, 72000, 1))
    pingLabelsControls.add(ping)

    val defaultNS = "api.example.com/?task="
    val tagNS = "NS:"

    val rmLinesNS = try {
      Source.fromFile(filenameRm).getLines.toArray filter (_.startsWith(tagNS))
    } catch {
      case e: Exception => Array(tagNS + defaultNS)
    }
    val ns = rmLinesNS.length match {
      case 0 => defaultNS
      case _ => rmLinesNS(0).split(":")(1)
    }

    pingLabelsControls.add(new JLabel("API URL", SwingConstants.RIGHT))
    val NSJSpinner = new JTextField(ns)
    pingLabelsControls.add(NSJSpinner)

    // volsOfDay count selection

    val volsOfDayControls = new JPanel
    volsOfDayControls.setLayout(new GridLayout(0, 12))
    allControls.add(volsOfDayControls)

    val filenameVolsOfDay = "task/VOLSOFDAY"
    val volsOfDayLines = try {
      Source.fromFile(filenameVolsOfDay).getLines.toArray filter (_.contains(";"))
    } catch {
      case e: Exception => (0 to 23).toArray.map("%02d".format(_) + ":00:00;0")
    }
    val volsOfDay = volsOfDayLines map (tv => tv.split(";")(1))

    var volsOfDayJSpinner = Array[JSpinner]()
    val n = (0 to 23).toArray
    for ((x, y) <- volsOfDay.zip(n)) {
      volsOfDayControls.add(new JLabel("%02d".format(y) + ":00", SwingConstants.RIGHT))

      volsOfDayJSpinner :+= new JSpinner(new SpinnerNumberModel(x.toInt, 0, 100, 1))
      volsOfDayControls.add(volsOfDayJSpinner.last)
    }

    // workDays count selection

    val workDaysControls = new JPanel
    workDaysControls.setLayout(new GridLayout(0, 7))
    allControls.add(workDaysControls)

    val weekLabelsText = Array[String]("Вос", "Пон", "Вто", "Сре", "Чет", "Пят", "Суб")
    var weekJCheckBox = Array[JCheckBox]()

    // wd_mute
    val filenameWdMute = "task/wd_mute"
    val wdMute = try {
      Source.fromFile(filenameWdMute).getLines.toArray
    } catch {
      case e: Exception => Array("1234560")
    }

    for (day <- weekLabelsText) {
      weekJCheckBox :+= new JCheckBox(day)
    }

    for (day <- wdMute(0)) {
      weekJCheckBox(day.toInt - 48).setSelected(true)
    }

    for (dayBox <- weekJCheckBox) {
      workDaysControls.add(dayBox)
    }

    val unmuteDaysControls = new JPanel
    unmuteDaysControls.setLayout(new GridLayout(0, 14))
    allControls.add(unmuteDaysControls)

    var unmuteDays = Array[JSpinner]()
    var muteDays = Array[JSpinner]()

    val filenameUnmute = "task/unmute"
    val filenameMute = "task/mute"

    val unmuteLines = try {
      Source.fromFile(filenameUnmute).getLines.toArray filter (_.contains(":"))
    } catch {
      case e: Exception => Array.fill(7)("00:00:00")
    }

    var value = 0
    for (day <- 0 to 6) {
      value = 0
      if (wdMute(0).contains(day.toString))
        value = unmuteLines(wdMute(0).indexOfSlice(day.toString)).substring(0, 2).toInt
      unmuteDays :+= new JSpinner(new SpinnerNumberModel(value, 0, 23, 1))
    }

    for (dayBox <- unmuteDays) {
      unmuteDaysControls.add(new JLabel("С      :", SwingConstants.RIGHT))
      unmuteDaysControls.add(dayBox)
    }

    val muteLines = try {
      Source.fromFile(filenameMute).getLines.toArray filter (_.contains(":"))
    } catch {
      case e: Exception => Array.fill(7)("23:59:59")
    }

    for (day <- 0 to 6) {
      value = 0
      if (wdMute(0).contains(day.toString))
        value = muteLines(wdMute(0).indexOfSlice(day.toString)).substring(0, 2).toInt
      muteDays :+= new JSpinner(new SpinnerNumberModel(value, 0, 23, 1))
    }

    for (dayBox <- muteDays) {
      unmuteDaysControls.add(new JLabel("По    :", SwingConstants.RIGHT))
      unmuteDaysControls.add(dayBox)
    }

    val filenameWtTaskId = "task/wt_taskid"
    val wtTaskId = try {
      Source.fromFile(filenameWtTaskId).getLines.toArray
    } catch {
      case e: Exception => Array("0")
    }

    val filesProgress = (new java.io.File("./task/PROGRESS")).listFiles
    val filesHere = if (filesProgress == null)
      (new java.io.File("./")).listFiles
    else
      filesProgress

    if (filesProgress == null) showMessageDialog(null, "~/task/PROGRESS недоступен", "Ошибка доступа", 2)

    val wtTaskTimesFiles =
      for (
        file <- filesHere
        if file.isFile
        if file.getName.endsWith(wtTaskId(0))
      ) yield file.getName.split("_")(0)

    val wtTaskIdControls = new JPanel
    wtTaskIdControls.setLayout(new GridLayout(0, 2))
    allControls.add(wtTaskIdControls)
    wtTaskIdControls.add(new JLabel("WORTKIME TASK ID:", SwingConstants.RIGHT))

    val numberWtTask =
      if (wtTaskTimesFiles.length == 0)
        0
      else
        wtTaskId(0).toInt

    val wtTaskIdJSpinner = new JSpinner(new SpinnerNumberModel(numberWtTask, 0, 100000000, 1))
    wtTaskIdControls.add(wtTaskIdJSpinner)

    val editor = new JSpinner.NumberEditor(wtTaskIdJSpinner, "#")
    wtTaskIdJSpinner.setEditor(editor)

    //tasks
    val searchInUpload = new JTextField("")
    plsControls.add(searchInUpload)

    val plsList = new JPanel
    plsList.setLayout(new GridLayout(0, 2))
    plsControls.add(plsList)


    val listModel = new DefaultListModel[String]
    val fileTestUpload = (new java.io.File("./upload")).listFiles
    if (fileTestUpload == null) showMessageDialog(null, "~/upload недоступен", "Ошибка доступа", 1)

    val filesUpload = if (fileTestUpload == null)
      Array[java.io.File]()
    else
      fileTestUpload filter (_.getName.contains(".mp3"))
    for (file <- filesUpload) {
      listModel.addElement(file.getName)
    }

    val plsModel = new DefaultListModel[String]

    val listUpload = new JList[String](listModel)
    listUpload.setLayoutOrientation(JList.VERTICAL)
    val listPls = new JList[String](plsModel)
    listPls.setLayoutOrientation(JList.VERTICAL)
    listPls.setVisibleRowCount(JList.VERTICAL);

    plsList.add(new JScrollPane(listUpload))
    plsList.add(new JScrollPane(listPls))

    val plsButton = new JPanel
    plsButton.setLayout(new GridLayout(2, 500))
    plsControls.add(plsButton)

    val addButton = new JButton("Добавить в список")
    addButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        for (index <- listUpload.getSelectedIndices())
          plsModel.addElement(listModel.getElementAt(index))
      }
    })
    plsButton.add(addButton)

    //  SaveButtons
    val saveControls = new JPanel
    saveControls.setLayout(new GridLayout(2, 100))
    add(saveControls)

    val saveButton = new JButton("Сохранить")
    saveButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {

        //save MUSIC vols
        printToFile(new File(filenameMusic)) { p =>
          p.print(vol(0).getValue + ":" + vol(1).getValue + ":" + vol(2).getValue)
        }

        //save JINGL vols
        printToFile(new File(filenameJingl)) { p =>
          p.print(vol(0).getValue + ":" + vol(3).getValue + ":" + vol(4).getValue)
        }

        // filenameEq  equalizer
        printToFile(new File(filenameEq)) { p =>
          p.print((equalizer map (_.getValue + ":")).mkString.dropRight(1))
        }

        // filenameRm tagId idPc
        printToFile(new File(filenameRm)) { p =>
          p.println(tagId + idPc)
        }

        // filenameRm tagInterval interval
        printToFile(new File(filenameRm)) { p =>
          p.println(tagInterval + interval)
        }

        // tagNs ns
        printToFile(new File(filenameRm)) { p =>
          p.println(tagNS + ns)
        }

        // filenameVolsOfDay volsOfDayJSpinner
        printToFile(new File(filenameVolsOfDay)) { p =>
          for {x <- n} p.println("%02d".format(x) + ":00:00;" + volsOfDayJSpinner(x).getValue.toString)
        }

        // filenameWdMute
        printToFile(new File(filenameWdMute)) { p =>
          for {x <- 1 to 6}
            if (weekJCheckBox(x).isSelected)
              p.print(x.toString)
          if (weekJCheckBox(0).isSelected)
            p.print("0")
        }

        // filenameUnmute unmuteDays
        printToFile(new File(filenameUnmute)) { p =>
          for {x <- 1 to 6}
            if (weekJCheckBox(x).isSelected)
              p.println("%02d".format(unmuteDays(x).getValue) + ":00:00")
          if (weekJCheckBox(0).isSelected)
            p.println("%02d".format(unmuteDays(0).getValue) + ":00:00")
        }

        // filenameMute muteDays
        printToFile(new File(filenameMute)) { p =>
          for {x <- 1 to 6}
            if (weekJCheckBox(x).isSelected)
              p.println("%02d".format(muteDays(x).getValue) + ":00:00")
          if (weekJCheckBox(0).isSelected)
            p.println("%02d".format(muteDays(0).getValue) + ":00:00")
        }

        for {x <- 1 to 6}
          if (weekJCheckBox(x).isSelected) {
            printToFile(new File("%02d".format(unmuteDays(x).getValue) + ":00:00" + "_" + wtTaskIdJSpinner.getValue)) { p =>
              p.println("")
            }
            printToFile(new File("%02d".format(muteDays(x).getValue) + ":00:00" + "_" + wtTaskIdJSpinner.getValue)) { p =>
              p.println("")
            }
          }


        println(idPcJSpinner.getValue.asInstanceOf[Int])
        for (index <- listUpload.getSelectedIndices())
          plsModel.addElement(listModel.getElementAt(index))

      }
    })

    //saveButton.setPreferredSize(new Dimension(5, 10))
    saveControls.add(saveButton)


    // Initial selection
    /*
      val initSelectionControls = new JPanel
      initSelectionControls.setLayout(new GridLayout(0, 1))
      allControls.add(initSelectionControls)

      val initialSelectionGroup = new ButtonGroup()

      val initSelectionLabel = new JLabel("Initial Color Selection:")
      initSelectionControls.add(initSelectionLabel)

      val uniformSamplingButton = new JRadioButton("Uniform Sampling")
      uniformSamplingButton.setSelected(true);
      initSelectionControls.add(uniformSamplingButton)

      val randomSamplingButton = new JRadioButton("Random Sampling")
      initSelectionControls.add(randomSamplingButton)

      val uniformChoiceButton = new JRadioButton("Uniform Choice")
      initSelectionControls.add(uniformChoiceButton)

      initialSelectionGroup.add(randomSamplingButton)
      initialSelectionGroup.add(uniformSamplingButton)
      initialSelectionGroup.add(uniformChoiceButton)

      // Initial means selection
      val convergenceControls = new JPanel
      convergenceControls.setLayout(new BoxLayout(convergenceControls, BoxLayout.Y_AXIS))
      allControls.add(convergenceControls)

      val convergenceGroup = new ButtonGroup()

      val convergenceLabel = new JLabel("Convergence criteria:")
      initSelectionControls.add(convergenceLabel)

      val criteriaControls = new JPanel
      criteriaControls.setLayout(new GridLayout(0, 2))
      convergenceControls.add(criteriaControls)

      val stepConvergenceButton = new JRadioButton("Steps")
      criteriaControls.add(stepConvergenceButton)

      val stepCountSpinner = new JSpinner(new SpinnerNumberModel(5, 1, 50, 1))
      criteriaControls.add(stepCountSpinner)

      val etaConvergenceButton = new JRadioButton("Eta")
      etaConvergenceButton.setSelected(true);
      criteriaControls.add(etaConvergenceButton)

      val etaCountSpinner = new JSpinner(new SpinnerNumberModel(0.001, 0.00001, 0.01, 0.00001))
      criteriaControls.add(etaCountSpinner)

      val snrConvergenceButton = new JRadioButton("Sound-to-noise")
      criteriaControls.add(snrConvergenceButton)

      val snrCountSpinner = new JSpinner(new SpinnerNumberModel(40, 10, 80, 1))
      criteriaControls.add(snrCountSpinner)

      convergenceGroup.add(stepConvergenceButton)
      convergenceGroup.add(etaConvergenceButton)
      convergenceGroup.add(snrConvergenceButton)

      // Action Buttons
      val actionControls = new JPanel
      actionControls.setLayout(new GridLayout(0, 2))
      allControls.add(actionControls)

      val stepbutton = new JButton("Apply filter")
      stepbutton.addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) {
          var status = ""

        }
      })
      actionControls.add(stepbutton)

      val clearButton = new JButton("Reload")
      clearButton.addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) {

        }
      })
      actionControls.add(clearButton)

      val info = new JTextArea("              ")
      info.setBorder(BorderFactory.createLoweredBevelBorder)
      rightpanel.add(info, BorderLayout.SOUTH)


      val mainMenuBar = new JMenuBar()

      val fileMenu = new JMenu("File")
      val openMenuItem = new JMenuItem("Open...")
      openMenuItem.addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) {
          val fc = new JFileChooser()
          if (fc.showOpenDialog(ScalaShopFrame.this) == JFileChooser.APPROVE_OPTION) {

          }
        }
      })
      fileMenu.add(openMenuItem)
      val saveMenuItem = new JMenuItem("Save...")
      saveMenuItem.addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) {
          val fc = new JFileChooser("epfl-view.png")
          if (fc.showSaveDialog(ScalaShopFrame.this) == JFileChooser.APPROVE_OPTION) {

          }
        }
      })
      fileMenu.add(saveMenuItem)
      val exitMenuItem = new JMenuItem("Exit")
      exitMenuItem.addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) {
          sys.exit(0)
        }
      })
      fileMenu.add(exitMenuItem)

      mainMenuBar.add(fileMenu)

      val helpMenu = new JMenu("Help")
      val aboutMenuItem = new JMenuItem("About")
      aboutMenuItem.addActionListener(new ActionListener {
        def actionPerformed(e: ActionEvent) {
          JOptionPane.showMessageDialog(null, "ScalaShop, the ultimate image manipulation tool\nBrought to you by EPFL, 2015")
        }
      })
      helpMenu.add(aboutMenuItem)

      mainMenuBar.add(helpMenu)

      setJMenuBar(mainMenuBar)

    */

    setVisible(true)
    /*
        def updateInformationBox(status: String, time: Double) {
          info.setText(s"$status\nTime: ${time.toInt} ms.")
        }

        def getColorCount: Int =
          colorCountSpinner.getValue.asInstanceOf[Int]
    */


  }

  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  } catch {
    case _: Exception => println("Cannot set look and feel, using the default one.")
  }

  val frame = new ScalaShopFrame

  def main(args: Array[String]) {
    frame.repaint()
  }

}
