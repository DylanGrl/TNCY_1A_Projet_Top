/**
  * Created by grill & guillaume & schuimer
  */


import com.tncy.top.files.WavWrapper;
import com.tncy.top.files.Utils;
import Complex._;
import Array._;
import Math._;
import java.io._
import java.util.StringTokenizer;

object ProjetTop extends App {

  ///////////////////////////////////////////
  // FONCTIONS ELEMENTAIRES NECESSAIRES    //
  ///////////////////////////////////////////

  /**
    * Ancienne version de la FFT trop lente
    *
    * @param fx la fonction dont on veut la Transformer
    * @return la transformer de la fonction
    */
  def tfd(fx: Array[Complex]): Array[Complex] = {
    var i: Complex = new Complex(0, 1);
    var pi: Double = Math.PI;
    var zero: Complex = 0;
    var n: Int = fx.length
    var frequences = Array.fill(n)(zero);
    //println("N="+n);
    if (n == 0) {
      return frequences;
    }
    else if (n == 1) {
      return fx;
    }
    else {
      for (j <- 0 to n - 1) {
        for (k <- 0 to n - 1) {
          //exp(i*x) = cos(x) + i*sin(x)
          var x = (2 * pi * (j) * ((k * 1.0) / (n * 1.0))) * (-1);
          //println("X="+x);
          //println("Sin(X)="+sin(x));
          //println("Cos(X)="+cos(x));
          frequences(j) = frequences(j) + fx(k) * (cos(x) + i * sin(x));
        }
      }
      return frequences
    }

  }

  def copy(src: Array[Complex], dst: Array[Complex]) = {
    if (dst.length < src.length) {
      println("Erreur de copie dst < src");
    }
    else {
      for (j <- 0 to src.length - 1) {
        dst(j) = src(j);
      }
    }
  }

  /**
    * A CORRIGER !!!!!!
    * Implémentation de la fonction du radix-2
    * Src : https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm
    *
    * @param fx La fonction dont on veut la transformé de Fourier
    * @return La transformé de Fourier de la fonction
    */
  def fft(fx: Array[Complex]): Array[Complex] = {
    //Longueur de la fonction (Array passé en entrée)
    var long = fx.length;
    //Création du nombre imaginaire i
    var i: Complex = new Complex(0, 1);
    //Initialisation de la valeur Pi
    var pi: Double = Math.PI;
    //Initialisation du nombre 0 en complexe
    var zero = new Complex(0);
    var N = long / 2;
    //On vérifie la longueur avant de commencer
    if (long == 1) {
      return Array(fx(0));
    }
    //On vérifie si la longueur est une puissance de 2 en observant la partie entière de log2(long)
    //On compare cette  valeur a celle de log2(long) si elle ne sont pas égale il ne s'agit pas d'une puissance de 2
    if (((log(long) / log(2))).toInt.toDouble != log(long) / log(2)) {
      println("N'a pas une longueur de puissance 2 !");
      //var rt = Array.fill(pow(2,(log(long)/log(2)).toInt +1).toInt)(zero);
      //copy(fx,rt);
      //afficheListC(fx);
      //afficheListC(rt);
      //Pour le moment dans le cas ou ce n'est pas une puissance de 2 on renvoie l'ancienne version de la tfd
      return tfd(fx);
    }
    else {
      //On commence l'algo du radix-2
      //On initialise n à N/2 (longueur de Rt / 2)
      var n = long / 2;
      var e = Array.fill(n)(zero);
      for (k <- 0 to n - 1) {
        e(k) = fx(2 * k);
      }
      var E = fft(e);

      var o = Array.fill(n)(zero);
      for (k <- 0 to n - 1) {
        o(k) = fx(2 * k + 1);
      }
      var O = fft(o);

      var y = Array.fill(fx.length)(zero);
      for (k <- 0 to n - 1) {
        var x = (2 * pi * ((k * 1.0) / (N * 2.0))) * (-1.0);
        y(k) = E(k) + (cos(x) + i * sin(x)) * O(k);
        y(k + n) = E(k) - (cos(x) + i * sin(x)) * O(k);
      }
      return y;
    }
  }

  /**
    * La constellation permet de récupérer les points cibles de la chanson et de créer un hash de ces valeurs
    *
    * @param song la musique en question
    * @return
    */
  def constellation(song: Array[Array[Int]]): Array[Array[Complex]] = {
    var musique = song(1);
    //Taille des intervalles parcouru
    val interval = 4096;
    //Longueur de la musique
    val longueur = musique.length;
    //Nombre d'échantillon de la taille d'un interval
    val nb_echantillon = longueur / interval;
    //Initialisation de la valeur zero en complexe
    val zero = new Complex(0, 0);
    //On initialise la constellation comme un tableau à 2 dimension interval - nb_echantillon
    var constel: Array[Array[Complex]] = Array.ofDim(interval, nb_echantillon);
    //println("Interval: " + interval);
    //println("Nombre d'échantillon: " + nb_echantillon);
    //On commence par parcourir l'ensemble des échantillons
    for (j <- 0 to nb_echantillon - 1) {
      //On itilise un tableau de 0 de la longueur d'un interval
      var times = Array.fill(interval)(zero);
      //On parcourt un interval
      for (k <- 0 to interval - 1) {
        //On attribut au tableau qu'on vient d'initialiser la valeur en complex de l'élément
        // de la musique
        times(k) = new Complex(musique(j * interval + k));
      }
      //On applique la FFT au tableau contenant les valeurs de la musique
      var complex_times = fft(times);
      //On attribut à la constellation en cours le résultat de la FFT de l'interval prélevé
      constel(j) = complex_times;
      //afficheListC(constel(j));
    }

    return constel;

  }


  /**
    * Renvoie les empreintes clés de la musiques
    *
    * @param const     constellation de la musique
    * @param f_musique frequence de la musique
    * @return Tableau avec triplet (amplitude, frequence, temps)
    */
  def empreinte_cle(const: Array[Array[Complex]], f_musique: Int): Array[List[Double]] = {
    //On créer une list contenant l'ensemble des valeur sélectionné (les amplitudes)
    var selected: List[Double] = List.empty;
    //De même pour les fréquences
    var freq: List[Double] = List.empty;
    //De même pour le temps
    var time: List[Double] = List.empty;
    //On initialise la taille de la fenêtre à la longueur d'une ligne de la constellation
    var win_size = const(0).length;
    //On définit le tableau contenant les fréquences d'amplitude max sur chaque intervalle
    var max_list: Array[Complex] = Array.fill(const.length)(new Complex(0, 0));
    //Ces boucles récupères les points maximums
    for (j <- 0 to const.length - 1) {
      for (k <- 0 to const(j).length - 1) {
        if (const(j).length == const(0).length) {
          if (const(j)(k) > max_list(j)) {
            max_list(j) = const(j)(k);
          }
        }
      }
    }
    //On récupère ici l'ensemble des points ayant une intensité supérieur à 80% de celle des max
    for (j <- 0 to const.length - 1) {
      for (k <- 0 to const(j).length - 1) {
        if (const(j).length == const(0).length) {
          if (const(j)(k).modulus > (max_list(j).modulus * 0.80)) {
            //On stock l'amplitude
            selected = (const(j)(k).modulus) :: selected;
            //On stock la fréquence
            freq = (k * (f_musique / win_size.toDouble)) :: freq;
            //On stock le temps
            time = (j * (const(j).length) + k).toDouble :: time;
          }
        }
      }
    }

    //ON REMET DANS L'ORDRE LES LISTS

    //println("\nAmplitude sélection(L=" + selected.length + ") :");
    selected = selected.reverse;
    //selected.mkString("-");
    //println(selected);

    //println("\nFréquence sélection(L=" + freq.length + ") :");
    freq = freq.reverse;
    //freq.mkString("-");
    //println(freq);

    //println("\nTime sélection(L=" + time.length + ") :");
    time = time.reverse;
    //time.mkString("-");
    //println(time);

    //On initialise le tableau de retour avec les différentes listes
    var rt: Array[List[Double]] = Array.ofDim(3);
    rt(0) = selected;
    rt(1) = freq;
    rt(2) = time;
    return rt;
  }


  /**
    * Convertie les musiques stéréos en mono si besoin
    *
    * @param musique en mono ou stéréo
    * @return musique en mono
    */
  def StereoToMono(musique: Array[Array[Int]]): Array[Array[Int]] = {
    if (musique(0)(1) == 1) {
      //println("Fichier MONO -> On le renvoie comme il est")
      return musique;
    }
    else {
      //println("Fichier Stéréo -> On fusionne les cannaux et on renvoie la nouvelle musique");
      //On récupère le canal1
      var canal_1 = musique(1);
      //Le 2eme
      var canal_2 = musique(2);
      //On initialise la nouvelle fréquence 1/2 de celle de base
      var new_freq = musique(0)(2) / 2;
      //Le nouveau nombre de canal (fusion donc 1)
      var new_canal = 1;
      //Initialisation du tableau des valeurs fusionnés
      var fusion: Array[Int] = Array.ofDim(canal_1.length);
      //Calcul des valeurs fusionnées
      for (j <- 0 to fusion.length - 1) {
        //Nouvelle_Valeur = (Canal 1 + Canal 2)/2
        fusion(j) = (canal_1(j) + canal_2(j)) / 2;
      }
      //On créer et on renvoie le nouveau son
      var Mono: Array[Array[Int]] = Array.ofDim(3, fusion.length);
      Mono(0)(0) = musique(0)(0);
      Mono(0)(1) = new_canal;
      Mono(0)(2) = fusion.length;
      Mono(1) = fusion;

      return Mono;

    }
  }

  /**
    * Fonction permettant l'harmonisation des fréquence à 11025Hz
    *
    * @param musique Musique de n'importe quelle fréquence
    * @return Musique de fréquence 11025Hz
    */
  def unification_frequence(musique: Array[Array[Int]]): Array[Array[Int]] = {
    if (musique(0)(0) == 11025) {
      //println("Signal de fréquence :" + musique(0)(0) + " Hz");
      return musique;
    }
    else if (musique(0)(0) == 22050) {
      //println("Signal de fréquence :" + musique(0)(0) + " Hz");
      var rt: Array[Array[Int]] = Array.ofDim(3, musique(1).length);
      rt(0)(0) = 11025;
      rt(0)(1) = musique(0)(1);
      var new_freq = musique(0)(2) / 2;
      rt(0)(2) = new_freq;
      rt(1) = Array.fill(musique(1).length / 2)(0);
      for (j <- 0 to rt(1).length - 1) {
        //On effectue la moyenne des 2 valeurs pour en créer qu'une seule
        rt(1)(j) = (musique(1)(2 * j) + musique(1)(2 * j + 1)) / 2
      }
      return rt;
    }
    else {
      //println("Signal de fréquence :" + musique(0)(0) + " Hz");
      var rt: Array[Array[Int]] = Array.ofDim(3, musique(1).length);
      rt(0)(0) = 11025;
      rt(0)(1) = musique(0)(1);
      var new_freq = musique(0)(2) / 4;
      rt(0)(2) = new_freq;
      rt(1) = Array.fill(musique(1).length / 4)(0);
      for (j <- 0 to rt(1).length - 1) {
        //On effectue la moyenne des 4 valeurs pour en créer qu'une seule
        rt(1)(j) = (musique(1)(4 * j) + musique(1)(4 * j + 1) + musique(1)(4 * j + 2) + musique(1)(4 * j + 3)) / 4
      }
      return rt;
    }
  }

  /**
    * Permet de décrire les composantes de la musique
    *
    * @param wav2D la musique
    */
  def description(wav2D: Array[Array[Int]]) = {
    println("The music's sample rate is: " + wav2D(0)(0) + " frames per second.");
    println("The music's number of channels is: " + wav2D(0)(1) + " channels.");
    println("The music's number of frames is: " + wav2D(0)(2) + " frames.");
  }

  ///////////////////////////////////////////
  // TEST DES DIFFERENTES FONCTIONS________//
  ///////////////////////////////////////////

  def test_FFT() = {
    println("Test de la FFT :")
    var test = Array(1.0, 2.0, 2.0, 3.0);
    afficheList(test);
    var testC = doubleToComplex(test);
    println("Radix-2 :");
    afficheListC(fft(testC));
    println("Normal FFT");
    afficheListC(tfd(testC));
  }

  def test_StereoToMono() = {
    var filePath: String = "./src/Echantillon/sample_stereo.wav";
    // Loads wrapped wav
    var wrappedWav: WavWrapper = new WavWrapper(filePath);
    // Gets the music content
    var wav2D: Array[Array[Int]] = wrappedWav.getWav();

    println("Fichier de base (Avant fusion)");
    description(wav2D);


    println("\nFichier modifier :");
    wav2D = StereoToMono(wav2D);
    description(wav2D);
  }

  def test_unification_frequences_11025() = {
    var filePath: String = "./src/Echantillon/sample_11025.wav";
    var wrappedWav: WavWrapper = new WavWrapper(filePath);
    var wav2D: Array[Array[Int]] = wrappedWav.getWav();

    println("Fichier de base (11025Hz)");
    description(wav2D);

    println("\nFichier modifier :");
    wav2D = unification_frequence(wav2D);
    description(wav2D);

    println("\n______________________________________\n")
  }

  def test_unification_frequences_22050() = {
    var filePath: String = "./src/Echantillon/sample_22050.wav";
    var wrappedWav: WavWrapper = new WavWrapper(filePath);
    var wav2D: Array[Array[Int]] = wrappedWav.getWav();

    println("Fichier de base (22050Hz)");
    description(wav2D);

    println("\nFichier modifier :");
    wav2D = unification_frequence(wav2D);
    description(wav2D);

    println("\n______________________________________\n")
  }

  def test_unification_frequence_44100() = {
    var filePath: String = "./src/Echantillon/sample_44100.wav";
    var wrappedWav: WavWrapper = new WavWrapper(filePath);
    var wav2D: Array[Array[Int]] = wrappedWav.getWav();

    println("Fichier de base (44100Hz)");
    description(wav2D);

    println("\nFichier modifier :");
    wav2D = unification_frequence(wav2D);
    description(wav2D);

    println("\n______________________________________\n")
  }

  def test_unification_frequence() = {
    test_unification_frequences_11025();
    test_unification_frequences_22050();
    test_unification_frequence_44100();
  }

  def test_recup_empreinte_file() = {
    var savePath: String = "./src/data/";
    var saves: Array[String] = Utils.listFiles(savePath);
    var file_to_get = savePath + saves(0);
    println("Récupération de : " + file_to_get);
    var e = recup_empreinte_file(file_to_get);
    println(e.mkString(" | "));
  }

  ///////////////////////////////////////////
  // FONCTIONS FINALES -> COEUR DU PROG    //
  ///////////////////////////////////////////

  def stockage_empreinte(musique: Array[Array[Int]], file: String): Boolean = {
    println("\nDescription du fichier :");
    description(musique);
    println("\n");
    var song = StereoToMono(musique);
    println("Conversion Stéréo to Mono terminée ->");
    var uni = unification_frequence(song);
    println("Harmonisation des fréquences terminée ->");
    var c = constellation(uni);
    println("Constellation terminée ->");
    //var s = simp_const(c);
    //println("Simplification de la constellation terminée ->");
    var e = empreinte_cle(c, musique(0)(0));
    println("Création des empreintes terminée ->");
    val writer = new PrintWriter(new File(file));
    for (j <- 0 to e(1).length - 1) {
      writer.write(e(1)(j).toString() + "_");
    }
    writer.close()
    println("Stockage de l'empreinte terminée ->");
    return true;
  }

  def launch_recherche() = {
    // Directory path
    var directoryPath: String = "./src/Echantillon/";

    // Gets the array of files and folders
    var files: Array[String] = Utils.listFiles(directoryPath);
    println("Veuillez choisir l'échantillon dans la liste :\n");
    for (j <- 0 to files.length - 1) {
      println(j + ") : " + files(j) + " and its path is " + directoryPath + files(j));
    }
    var line = scala.io.StdIn.readLine()
    println("\nVous avez sélectionné : " + files(line.toInt));
    var musique_path = files(line.toInt);
    var wrappedWav: WavWrapper = new WavWrapper(directoryPath + "/" + musique_path);
    var musique: Array[Array[Int]] = wrappedWav.getWav();

    println("\nDescription du fichier :");
    description(musique);
    println("\n");
    var song = StereoToMono(musique);
    println("Conversion Stéréo to Mono terminée ->");
    var uni = unification_frequence(song);
    println("Harmonisation des fréquences terminée ->");
    var c = constellation(uni);
    println("Constellation terminée ->");
    var e = empreinte_cle(c, musique(0)(0));
    println("Création des empreintes terminée ->\n");
    recherche_titre(e(1));

    println("Relancer le programme ?");
    println("1)Oui\n2)Non");
    val rep = scala.io.StdIn.readLine()
    if (rep.toInt == 1) {
      ShaTncy();
    }
    else {
      System.exit(0);
    }

  }

  def launch_ajout() = {
    // Directory path
    var directoryPath: String = "./src/Full_song/";
    var savePath: String = "./src/data/";

    // Gets the array of files and folders
    var files: Array[String] = Utils.listFiles(directoryPath);
    var saves: Array[String] = Utils.listFiles(savePath);
    println("\nAJOUT D'UN FICHIER\n");
    println("Liste fichier existant déjà :");
    for (j <- 0 to saves.length - 1) {
      println(j + ") : " + files(j) + ".txt");
    }
    println("\nVeuillez choisir le fichier dans la liste :");
    for (j <- 0 to files.length - 1) {
      println(j + ") : " + files(j) + " and its path is " + directoryPath + files(j));
    }
    val line = scala.io.StdIn.readLine()
    println("\nVous avez sélectionné : " + files(line.toInt));
    var saveFile = files(line.toInt) + ".txt";
    var full_save_path = savePath + saveFile;
    println("Son empreinte serra sauvegardé dans :" + full_save_path);
    var musique_path = files(line.toInt);
    var wrappedWav: WavWrapper = new WavWrapper(directoryPath + "/" + musique_path);
    var musique: Array[Array[Int]] = wrappedWav.getWav();
    var sto = stockage_empreinte(musique, full_save_path);
    if (sto) {
      println("\nVotre fichier à bien était stocké");
    }
    else {
      println("Erreur dans le stockage du fichier");
    }

    println("\nRelancer le programme ?");
    println("1)Oui\n2)Non");
    val rep = scala.io.StdIn.readLine()
    if (rep.toInt == 1) {
      ShaTncy();
    }
    else {
      System.exit(0);
    }

  }

  def create_full_data() = {
    // Directory path
    var directoryPath: String = "./src/Full_song/";
    var savePath: String = "./src/data/";

    // Gets the array of files and folders
    var files: Array[String] = Utils.listFiles(directoryPath);
    println("\nCREATION DE LA BDD COMPLETE\n");
    for (j <- 0 to files.length - 1) {
      var saveFile = files(j) + ".txt";
      var full_save_path = savePath + saveFile;
      println("\nNouveau Fichier - L'empreinte serra sauvegardé dans :" + full_save_path);
      var musique_path = files(j);
      var wrappedWav: WavWrapper = new WavWrapper(directoryPath + "/" + musique_path);
      var musique: Array[Array[Int]] = wrappedWav.getWav();
      var sto = stockage_empreinte(musique, full_save_path);
      if (sto) {
        println("\nVotre fichier à bien était stocké");
      }
      else {
        println("Erreur dans le stockage du fichier");
      }
    }

    println("\nRelancer le programme ?");
    println("1)Oui\n2)Non");
    val rep = scala.io.StdIn.readLine()
    if (rep.toInt == 1) {
      ShaTncy();
    }
    else {
      System.exit(0);
    }

  }

  def recherche_titre(empreinte_echant: List[Double]) = {
    var savePath: String = "./src/data/";
    var saves: Array[String] = Utils.listFiles(savePath);
    var nb_cocondordence: Array[Int] = Array.ofDim(saves.length);
    for (num_musique <- 0 to saves.length - 1) {
      var current_fichier = recup_empreinte_file(savePath + saves(num_musique));
      for (index_current_echant <- 0 to empreinte_echant.length - 1) {
        for (index_current_fichier <- 0 to current_fichier.length - 1) {
          if (empreinte_echant(index_current_echant) == current_fichier(index_current_fichier)) {
            nb_cocondordence(num_musique) = nb_cocondordence(num_musique) + 1;
          }
        }
      }
    }
    var index_max = 0;
    var nb_simi = 0;
    for (j <- 0 to nb_cocondordence.length - 1) {
      if (nb_cocondordence(j) > nb_simi) {
        index_max = j;
        nb_simi = nb_cocondordence(j);
      }
    }
    var best_choice = saves(index_max);
    println("La musique semble être : " + best_choice);
  }

  def recup_empreinte_file(file_path: String): List[Double] = {
    var e: List[Double] = List.empty;
    val bufferedSource = io.Source.fromFile(file_path)
    for (line <- bufferedSource.getLines) {
      val cols = line.split("_").map(_.trim);
      for (j <- 0 to cols.length - 1) {
        e = cols(j).toDouble :: e;
      }
    }
    bufferedSource.close
    return e.reverse;
  }

  def ShaTncy(): Unit = {
    println("_____________________________________");
    println("| ShaTncy Grill,Guillaume & Schuimer |");
    println("_____________________________________\n");

    println("CHOISIR CE QUE VOUS SOUHAITEZ FAIRE : ");
    println("1) Chercher le titre d'une musique");
    println("2) Ajouter une musique dans la BDD");
    println("3) Créer la base de donnée complète");
    println("4) Quitter")
    val line = scala.io.StdIn.readLine()
    if (line.toInt == 1) {
      launch_recherche();
    }
    else if (line.toInt == 2) {
      launch_ajout();
    }
    else if (line.toInt == 3) {
      create_full_data();
    }
    else if (line.toInt == 4) {
      System.exit(0);
    }
    else {
      ShaTncy();
    }
  }


  //test_FFT();
  //test_StereoToMono();
  //test_unification_frequence();
  //test_recup_empreinte_file();
  ShaTncy();

}
