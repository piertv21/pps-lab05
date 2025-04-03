package ex

import util.Optionals.Optional
import util.Sequences.* // Assuming Sequence and related methods are here

// Represents a course offered on the platform
trait Course:
  def courseId: String // Unique identifier (e.g., "CS101", "SCALA01")
  def title: String
  def instructor: String
  def category: String // e.g., "Programming", "Data Science", "Design"

object Course:

  // Course implementation
  private case class CourseImpl(courseId: String, title: String, instructor: String, category: String) extends Course

  // Factory method for creating Course instances
  def apply(courseId: String, title: String, instructor: String, category: String): Course =
    CourseImpl(courseId, title, instructor, category)

  val empty = apply("", "", "", "") // Empty course instance for convenience
/**
 * Manages courses and student enrollments on an online learning platform.
 */
trait OnlineCoursePlatform:
  /**
   * Adds a new course to the platform's catalog.
   * @param course The course to add.
   */
  def addCourse(course: Course): Unit

  /**
   * Finds courses belonging to a specific category.
   * @param category The category to search for.
   * @return A sequence of courses in that category.
   */
  def findCoursesByCategory(category: String): Sequence[Course]

  /**
   * Retrieves a specific course by its unique ID.
   * @param courseId The ID of the course to retrieve.
   * @return An Optional containing the course if found, otherwise Optional.empty.
   */
  def getCourse(courseId: String): Optional[Course]

  /**
   * Removes a course from the platform's catalog.
   * (Note: This basic version doesn't handle cascading removal of enrollments).
   * @param course The course to remove.
   */
  def removeCourse(course: Course): Unit

  /**
   * Checks if a course with the given ID exists in the catalog.
   * @param courseId The ID to check.
   * @return true if the course exists, false otherwise.
   */
  def isCourseAvailable(courseId: String): Boolean

  /**
   * Enrolls a student in a specific course.
   * Assumes studentId is unique for each student.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course to enroll in.
   *                 Fails silently if the course doesn't exist.
   */
  def enrollStudent(studentId: String, courseId: String): Unit

  /**
   * Unenrolls a student from a specific course.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course to unenroll from.
   */
  def unenrollStudent(studentId: String, courseId: String): Unit

  /**
   * Retrieves all courses a specific student is enrolled in.
   * @param studentId The ID of the student.
   * @return A sequence of courses the student is enrolled in.
   */
  def getStudentEnrollments(studentId: String): Sequence[Course]

  /**
   * Checks if a student is enrolled in a specific course.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course.
   * @return true if the student is enrolled, false otherwise.
   */
  def isStudentEnrolled(studentId: String, courseId: String): Boolean

end OnlineCoursePlatform

object OnlineCoursePlatform:

  // OnlineCoursePlatform implementation (using only Optionals, Sequences and Streams)
  private class OnlineCoursePlatformImpl extends OnlineCoursePlatform:

    // Enrollment data structure
    private case class Enrollment(studentId: String, courseId: String)

    // Internal state representation
    private var courses: Sequence[Course] = Sequence.empty
    private var enrollments: Sequence[Enrollment] = Sequence.empty
    
    def addCourse(course: Course): Unit =
      if !courses.contains(course) then
        courses = courses.concat(Sequence(course))
        
    def findCoursesByCategory(category: String): Sequence[Course] =
      courses.filter(_.category == category)
      
    def getCourse(courseId: String): Optional[Course] =
      courses.find(_.courseId == courseId)

    def removeCourse(course: Course): Unit =
      courses = courses.filter(_.courseId != course.courseId)
      
    def isCourseAvailable(courseId: String): Boolean =
      courses.filter(_.courseId == courseId) != Sequence.empty
      
    def enrollStudent(studentId: String, courseId: String): Unit =
      if isCourseAvailable(courseId) && !isStudentEnrolled(studentId, courseId) then
        enrollments = enrollments.concat(Sequence(Enrollment(studentId, courseId)))
        
    def unenrollStudent(studentId: String, courseId: String): Unit =
      enrollments = enrollments.filter(e => e.studentId != studentId || e.courseId != courseId)
      
    def getStudentEnrollments(studentId: String): Sequence[Course] =
      enrollments.filter(_.studentId == studentId).map(e => getCourse(e.courseId).orElse(Course.empty))
    
    def isStudentEnrolled(studentId: String, courseId: String): Boolean =
      enrollments.filter(e => e._1 == studentId && e._2 == courseId) != Sequence.empty
        
  // Factory method for creating an empty platform instance
  def apply(): OnlineCoursePlatform = new OnlineCoursePlatformImpl

/**
 * Represents an online learning platform that offers courses and manages student enrollments.
 * Hints:
 * - Start by implementing the Course trait.
 *    - A case class might be a good fit for this.
 * - Implement the OnlineCoursePlatform trait.
 *    - Focus on how to represent the internal state
 *    - Two main entities: courses and student enrollments
 *    - Set for courses? List of enrollments?
 *  - Implement the factory method for creating an empty platform instance.
 *  - Now start incrementally following the main given
 *
 */
@main def mainPlatform(): Unit =
  val platform = OnlineCoursePlatform()

  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")

  println(s"Is SCALA01 available? ${platform.isCourseAvailable(scalaCourse.courseId)}") // false
  platform.addCourse(scalaCourse)
  println(s"Is SCALA01 available? ${platform.isCourseAvailable(scalaCourse.courseId)}") // true
  platform.addCourse(pythonCourse)
  platform.addCourse(designCourse)

  println(s"Programming courses: ${platform.findCoursesByCategory("Programming")}") // Sequence(scalaCourse, pythonCourse)
  println(s"Design courses: ${platform.findCoursesByCategory("Design")}") // Sequence(designCourse)
  println(s"History courses: ${platform.findCoursesByCategory("History")}") // Sequence.empty

  println(s"Get SCALA01: ${platform.getCourse("SCALA01")}") // Optional.Just(scalaCourse)
  println(s"Get UNKNOWN01: ${platform.getCourse("UNKNOWN01")}") // Optional.Empty

  // Enrollments
  val studentAlice = "Alice123"
  val studentBob = "Bob456"

  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // false
  platform.enrollStudent(studentAlice, "SCALA01")
  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // true
  platform.enrollStudent(studentAlice, "DESIGN01")
  platform.enrollStudent(studentBob, "SCALA01") // Bob also enrolls in Scala

  println(s"Alice's enrollments: ${platform.getStudentEnrollments(studentAlice)}") // Sequence(scalaCourse, designCourse) - Order might vary
  println(s"Bob's enrollments: ${platform.getStudentEnrollments(studentBob)}") // Sequence(scalaCourse)

  platform.unenrollStudent(studentAlice, "SCALA01")
  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // false
  println(s"Alice's enrollments: ${platform.getStudentEnrollments(studentAlice)}") // Sequence(designCourse)

  // Removal
  platform.removeCourse(pythonCourse)
  println(s"Is PYTHON01 available? ${platform.isCourseAvailable(pythonCourse.courseId)}") // false
  println(s"Programming courses: ${platform.findCoursesByCategory("Programming")}") // Sequence(scalaCourse)