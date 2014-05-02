import collection.mutable.ListBuffer
import collection.JavaConversions._

import laika.directive.Directives._
import laika.util.Builders._
import laika.tree.Elements._
import laika.tree.Templates._
import laika.tree.Documents._
import laika.tree.Templates.rewriteRules
import laika.sbt.LaikaSbtPlugin.LaikaKeys._


object LaikaExtension
{
  def _pureToc (depth: Option[Int], rootConfig: String, title: Option[String], context: DocumentContext): Block = {
    
    val format = StringBullet("*")
    val maxLevel = depth getOrElse Int.MaxValue

	def isCurrent (path: Path) = path == context.document.path

	def indent( level: Int ) = Text( " "*((level - 1)*4) )

    def sectionToLink (section: SectionInfo, path: Path, level: Int) = {
      val title = section.title.content
      
      if (isCurrent( path ))
        InternalLink(/*indent(level) +: */title, section.id)
      else
        CrossLink(/*indent(level) +: */title, section.id, PathInfo(path, path.relativeTo(context.parent.path)))
    }
      
    def docToLink (document: Document, level: Int) = {
      val title = document.title
      
      if (isCurrent(document.path))
        CrossLink(/*indent(level) +: */title, "", PathInfo(document.path, document.path.relativeTo(context.parent.path)), options = Styles("active"))
      else
        CrossLink(/*indent(level) +: */title, "", PathInfo(document.path, document.path.relativeTo(context.parent.path)))
	}
	
    def treeToText (tree: DocumentTree, level: Int) =
      SpanSequence(tree.title)

	def _sectionsToList (sections: Seq[SectionInfo], path: Path, level: Int): List[ListItem] =
		if (sections.isEmpty || level > maxLevel)
			Nil
		else
		{
		val items = new ListBuffer[ListItem]

			for (section <- sections)
			{
				items += BulletListItem( List(SpanSequence(List(sectionToLink(section, path, level)))), format, options = Styles("level" + level) )
				items ++= _sectionsToList( section.content, path, level + 1 )
			}
		
			items.toList
		}
	
	def sectionsToList (sections: Seq[SectionInfo], path: Path, level: Int): Block =
		BulletList( _sectionsToList(sections, path, level), format, options = Styles("level" + level) )

    def include (nav: Navigatable): Boolean = nav match {
      case _:Document => true
      case tree: DocumentTree => tree.navigatables.exists(include(_))
    }

	def navigatablesToList (navigatables: Seq[Navigatable], level: Int): Block =
	{
	val items = new ListBuffer[ListItem]
	
		if (level <= maxLevel)
		{
			for (navigatable <- navigatables if include( navigatable ))
				navigatable match
				{
					case doc: Document =>
						if (doc.title.map( _.asInstanceOf[TextContainer].content ).mkString != "")
						{
							items += BulletListItem( List(SpanSequence(List(docToLink(doc, level)))), format, options = Styles("level" + level) )
							items ++= _sectionsToList( doc.sections, doc.path, level + 1 )
						}
				}
		}

		BulletList( items.toList, format )
	}

    val root = rootConfig match {
      case "#rootTree"        => context.root
      case "#currentTree"     => context.parent
      case "#currentDocument" => context.document
      case pathString => {
        val configPath = Path(pathString)
        val path = 
          (if (configPath.isAbsolute) configPath
          else (context.parent.path / configPath)).relativeTo(context.root.path) 
        context.root.selectDocument(path).getOrElse(context.root.selectSubtree(path).getOrElse(context.root))
      }
    }
    
    root match {
      case doc: Document      => sectionsToList(doc.sections, doc.path, 1)
      case tree: DocumentTree => navigatablesToList(tree.navigatables, 1)
    }
  }

  /** Implementation of the `toc` directive for templates.
   */
  val pureToc = Templates.create("pureToc") {
    import Templates.Combinators._
    import Templates.Converters._
    
    (attribute("depth", positiveInt).optional ~ 
        attribute("root").optional ~ 
        attribute("title").optional ~ 
        context) {  
      (depth, rootConfig, title, context) =>
        TemplateElement(_pureToc(depth, rootConfig.getOrElse("#rootTree"), title, context))
    }
  }

  def _bootstrapToc (depth: Option[Int], rootConfig: String, title: Option[String], context: DocumentContext): Block = {
    
    val format = StringBullet("*")
    val maxLevel = depth getOrElse Int.MaxValue

    def isCurrent (path: Path) = path == context.document.path
    
    def sectionToLink (section: SectionInfo, path: Path, level: Int) = {
      val title = section.title.content
      
      if (isCurrent( path ))
        SpanSequence(List(InternalLink(title, section.id)))
      else
        SpanSequence(List(CrossLink(title, section.id, PathInfo(path, path.relativeTo(context.parent.path)))))
    }

	def docToLink (document: Document, level: Int) =
	{
	val title = document.title

		if (isCurrent(document.path))
			SpanSequence(List(CrossLink(title, "", PathInfo(document.path, document.path.relativeTo(context.parent.path)), options = Styles("active"))))
		else
			SpanSequence(List(CrossLink(title, "", PathInfo(document.path, document.path.relativeTo(context.parent.path)))))
	}

    def treeToText (tree: DocumentTree, level: Int) =
      SpanSequence(tree.title)

    def sectionsToList (sections: Seq[SectionInfo], path: Path, level: Int): List[Block] =
      if (sections.isEmpty || level > maxLevel) Nil else {
        val items = for (section <- sections) yield 
            BulletListItem(sectionToLink(section, path, level) :: sectionsToList(section.content, path, level + 1), format)
        List(BulletList(items, format, options = Styles("nav", "level" + level)))
    }
    
    def include (nav: Navigatable): Boolean = nav match {
      case _:Document => true
      case tree: DocumentTree => tree.navigatables.exists(include(_))
    }

	def navigatablesToList (navigatables: Seq[Navigatable], level: Int): List[Block] =
	{
		if (level > maxLevel)
			Nil
		else
		{
		val items = new ListBuffer[BulletListItem]

			for (navigatable <- navigatables if include(navigatable))
				navigatable match
				{
					case doc: Document =>
						if (doc.title.map( _.asInstanceOf[TextContainer].content ).mkString != "")
							items += BulletListItem(docToLink(doc, level) :: sectionsToList(doc.sections, doc.path, level + 1), format)
					case tree: DocumentTree =>
						items += BulletListItem(treeToText(tree, level) :: navigatablesToList(tree.navigatables, level + 1), format)
				}

			List(BulletList(items.toList, format, options = Styles("nav", "level" + level)))
		}
	}

    val root = rootConfig match {
      case "#rootTree"        => context.root
      case "#currentTree"     => context.parent
      case "#currentDocument" => context.document
      case pathString => {
        val configPath = Path(pathString)
        val path = 
          (if (configPath.isAbsolute) configPath
          else (context.parent.path / configPath)).relativeTo(context.root.path) 
        context.root.selectDocument(path).getOrElse(context.root.selectSubtree(path).getOrElse(context.root))
      }
    }
    
    val list = root match {
      case doc: Document      => sectionsToList(doc.sections, doc.path, 1)
      case tree: DocumentTree => navigatablesToList(tree.navigatables, 1)
    }
    title match {
      case Some(text) => TitledBlock(List(Text(text)), list, Styles("toc"))
      case None       => BlockSequence(list, Styles("toc"))
    }
  }
  
  /** Implementation of the `toc` directive for templates.
   */
  val bootstrapToc = Templates.create("bootstrapToc") {
    import Templates.Combinators._
    import Templates.Converters._
    
    (attribute("depth", positiveInt).optional ~ 
        attribute("root").optional ~ 
        attribute("title").optional ~ 
        context) {  
      (depth, rootConfig, title, context) =>
        TemplateElement(_bootstrapToc(depth, rootConfig.getOrElse("#rootTree"), title, context))
    }
  }
}