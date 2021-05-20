PROGRAM CIRA86

    ! COPYRIGHT 2021 MELWYN FRANCIS CARLO

    ! COMPILE AND EXECUTE BY USING THE FOLLOWING TERMINAL COMMANDS:
    ! gfortran cira86.f90 -o cira86
    ! ./cira86

    IMPLICIT NONE

    ! VARIABLE DECLARATIONS

    INTEGER                     ::  INPUT_TYPE,                                 &
                                    OUTPUT_TYPE,                                &
                                    INPUT_MONTH,                                &
                                    I_ERROR

    REAL                        ::  INPUT_PRESSURE,                             &
                                    INPUT_HEIGHT,                               &
                                    INPUT_LATITUDE

    CHARACTER(LEN = 50)         ::  OUTPUT_TYPE_OPTION_3,                       &
                                    TEMP_STRING

    CHARACTER(LEN =  8)         ::  FILENAME

    REAL, DIMENSION(25)         ::  IEX, INVERSE_HEIGHT
    REAL, DIMENSION(70)         ::  SCALE_HEIGHT
    REAL, DIMENSION(71)         ::  PRESSURE, H
    REAL, DIMENSION(21, 17)     ::  PP
    REAL, DIMENSION(2, 25, 17)  ::  TW
    REAL, DIMENSION(3, 71, 33)  ::  Z

    INTEGER                     ::  I,                                          &
                                    X,                                          &
                                    JJ,                                         &
                                    PKI,                                        &
                                    I_MAX,                                      &
                                    L_BEG,                                      &
                                    L_INC,                                      &
                                    L_STEP

    ! CONSTANT VARIABLE DECLARATIONS AND DEFINITIONS

    CHARACTER(LEN = 10), PARAMETER :: OUTPUT_FORMAT =                           &
    '(A, F10.5)'

    CHARACTER(LEN = 2), DIMENSION(2), PARAMETER  :: FILENAME_PREFIX =           &
    [                                                                           &
        'cp', 'ch'                                                              &
    ]

    CHARACTER(LEN = 9), DIMENSION(12), PARAMETER :: MONTHS_LIST =               &
    [                                                                           &
        'JANUARY  ', 'FEBRUARY ', 'MARCH    ', 'APRIL    ', 'MAY      ',        &
        'JUNE     ', 'JULY     ', 'AUGUST   ', 'SEPTEMBER', 'OCTOBER  ',        &
        'NOVEMBER ', 'DECEMBER '                                                &
    ]

    CHARACTER(LEN = 8), DIMENSION(2),  PARAMETER :: INPUT_TYPES =               &
    [                                                                           &
        'PRESSURE', 'HEIGHT  '                                                  &
    ]

    CHARACTER(LEN = 21), DIMENSION(3) :: OUTPUT_TYPES =                         &
    [                                                                           &
        'TEMPERATURE          ', 'ZONAL WIND           ',                       &
        '                     '                                                 &
    ]

    ! INTRODUCTION

    PRINT  *,  NEW_LINE('1'),                                                   &
               NEW_LINE('1'),                                                   &
               NEW_LINE('1'),                                                   &
             ' #############################################################',  &
               NEW_LINE('1'),                                                   &
             ' ##                                                         ##',  &
               NEW_LINE('1'),                                                   &
             ' ##     COSPAR INTERNATIONAL REFERENCE ATMOSPHERE - 1986    ##',  &
               NEW_LINE('1'),                                                   &
             ' ##   ----------------------------------------------------  ##',  &
               NEW_LINE('1'),                                                   &
             ' ##                     0 TO 120 KILO-METERS                ##',  &
               NEW_LINE('1'),                                                   &
             ' ##                                                         ##',  &
               NEW_LINE('1'),                                                   &
             ' #############################################################',  &
               NEW_LINE('1')

    ! USER-INPUT FOR INPUT TYPE

    PRINT  *,                         NEW_LINE('1'),                            &
                                    ' ENTER 1 FOR PRESSURE-BASED INPUT.',       &
                                      NEW_LINE('1'),                            &
                                    ' ENTER 2 FOR HEIGHT-BASED INPUT.',         &
                                      NEW_LINE('1')

    WRITE (*, '(A)', ADVANCE='NO')  ' ENTER INPUT TYPE:  '

    READ  (*, '(I1)', IOSTAT=I_ERROR) INPUT_TYPE

    ! USER-INPUT VALIDATION FOR INPUT TYPE

    IF (I_ERROR /= 0) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    IF (INPUT_TYPE == 1) THEN
        L_INC                     =   5
        I_MAX                     =   71
        OUTPUT_TYPES(3)           = ' GEO-POTENTIAL HEIGHT'
        OUTPUT_TYPE_OPTION_3      = ' ENTER 3 FOR GEO-POTENTIAL HEIGHT'// & 
                                    ' (IN KILO-METRES).'
    ELSE IF (INPUT_TYPE == 2) THEN
        L_INC                     =   10
        I_MAX                     =   25
        OUTPUT_TYPES(3)           = ' PRESSURE            '
        OUTPUT_TYPE_OPTION_3      = ' ENTER 3 FOR PRESSURE (IN MILLI-BARS).'
    ELSE
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    ! USER-INPUT FOR OUTPUT TYPE

    PRINT *,                          NEW_LINE('1'),                            &
                                      NEW_LINE('1'),                            &
                                    ' ENTER 1 FOR TEMPERATURE (IN KELVIN).',    &
                                      NEW_LINE('1'),                            &
                                    ' ENTER 2 FOR ZONAL WIND',                  &
                                    ' (IN METERS PER SECOND).',                 &
                                      NEW_LINE('1'),                            &
                                      OUTPUT_TYPE_OPTION_3,                     &
                                      NEW_LINE('1')

    WRITE (*, '(A)', ADVANCE='NO')  ' ENTER OUTPUT TYPE:  '

    READ  (*, '(I1)', IOSTAT=I_ERROR) OUTPUT_TYPE

    ! USER-INPUT VALIDATION FOR OUTPUT TYPE

    IF (I_ERROR /= 0) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    IF (OUTPUT_TYPE /= 1 .AND. OUTPUT_TYPE /= 2 .AND. OUTPUT_TYPE /= 3) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    IF (INPUT_TYPE == 2 .AND. OUTPUT_TYPE == 3) THEN
        I_MAX = 21
    END IF

    ! USER-INPUT FOR INPUT MONTH

    PRINT *,                          NEW_LINE('1'),                            &
                                      NEW_LINE('1'),                            &
                                    ' ENTER 1 FOR JANUARY,',                    &
                                    ' 2 FOR FEBRUARY, ...,',                    &
                                    ' 12 FOR DECEMBER.',                        &
                                      NEW_LINE('1')

    WRITE (*, '(A)', ADVANCE='NO')  ' ENTER MONTH:  '

    READ  (*, '(I2)', IOSTAT=I_ERROR) INPUT_MONTH

    ! USER-INPUT VALIDATION FOR INPUT MONTH

    IF (I_ERROR /= 0) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    IF (INPUT_MONTH < 1 .OR. INPUT_MONTH > 12) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    PRINT *
    PRINT *

    ! USER-INPUT FOR INPUT PRESSURE OR INPUT HEIGHT

    IF (INPUT_TYPE == 1) THEN

        TEMP_STRING =  'ENTER PRESSURE VALUE (IN MILLI-BARS):'
        WRITE (*, '(A)', ADVANCE='NO')   ' '//TRIM(TEMP_STRING)//'  '
        READ  (*,   *,   IOSTAT=I_ERROR)      INPUT_PRESSURE

    ELSE IF (INPUT_TYPE == 2) THEN

        TEMP_STRING = 'ENTER GEO-POTENTIAL HEIGHT VALUE (IN KILO-METERS):'
        WRITE (*, '(A)', ADVANCE='NO')   ' '//TRIM(TEMP_STRING)//'  '
        READ  (*,   *,   IOSTAT=I_ERROR)      INPUT_HEIGHT

    END IF

    ! USER-INPUT VALIDATION FOR INPUT PRESSURE OR INPUT HEIGHT

    IF (I_ERROR /= 0) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    ! USER-INPUT FOR INPUT LATITUDE

    PRINT *,                          NEW_LINE('1'),                            &
                                      NEW_LINE('1'),                            &
                                    ' MUST BE IN THE INTERVAL [-80.0, 80.0]'

    WRITE (*, '(A)', ADVANCE='NO')  ' ENTER LATITUDE VALUE (IN DEGREES):  '

    READ  (*, *, IOSTAT=I_ERROR)      INPUT_LATITUDE

    ! USER-INPUT VALIDATION FOR INPUT LATITUDE

    IF (I_ERROR /= 0) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    IF (ABS(INPUT_LATITUDE) > 80.0) THEN
        PRINT *,                      NEW_LINE('1'),                            &
                                    ' ERROR: INVALID INPUT. ENDING PROGRAM.',   &
                                      NEW_LINE('1')
        RETURN
    END IF

    L_BEG                          =  INT((INPUT_LATITUDE + 80) / L_INC) + 1

    ! OPEN ONE OF THE VAX/VMS-STYLE BINARY DATA (*.DAT) FILES

    WRITE (FILENAME, '(A2, I2, A4)')  FILENAME_PREFIX(INPUT_TYPE),              &
                                      INPUT_MONTH + 10,                         &
                                      '.dat'

    OPEN (UNIT=1,                                                               &
          STATUS='OLD',                                                         &
          IOSTAT=I_ERROR,                                                       &
          FORM='UNFORMATTED',                                                   &
          FILE=TRIM(FILENAME))

    ! FILE OPEN VALIDATION

    IF (I_ERROR /= 0) THEN
        PRINT *,                  NEW_LINE('1'),                                &
                                ' ERROR: THE FILENAME LABELLED ',               &
                                  ACHAR(39),                                    &
                                  TRIM(FILENAME),                               &
                                  ACHAR(39),                                    &
                                  NEW_LINE('1'),                                &
                                ' EITHER DOES NOT EXIST OR HAS BEEN',           &
                                ' DOCTORED.',                                   &
                                  NEW_LINE('1'),                                &
                                ' PLEASE DOWNLOAD AGAIN A FRESH COPY OF',       &
                                ' THE MENTIONED FILE.',                         &
                                  NEW_LINE('1')
        RETURN
    END IF

    ! READ FILE CONTENTS INTO VARIABLES

    IF (INPUT_TYPE == 1) THEN
        READ (1)                  SCALE_HEIGHT,                                 &
                                  PRESSURE,                                     &
                                  H,                                            &
                                  Z
    ELSE IF (INPUT_TYPE == 2) THEN
        READ (1)                  INVERSE_HEIGHT,                               &
                                  TW,                                           &
                                  PP,                                           &
                                  IEX
    END IF

    ! CLOSE FILE

    CLOSE (1)

    ! PERFORMING THE REQUIRED CALCULATIONS

    I = 1

    IF (INPUT_TYPE == 1) THEN

        DO WHILE (I <= I_MAX .AND. PRESSURE(I) <= INPUT_PRESSURE)
            I   = I + 1
        END DO

    ELSE IF (INPUT_TYPE == 2) THEN

        DO WHILE (I <= I_MAX .AND. PKI >= INPUT_HEIGHT)
            I   = I + 1
            PKI = INVERSE_HEIGHT(I)
        END DO

    END IF

    JJ          = L_BEG
    L_STEP      = 1

    IF (INPUT_TYPE == 1) THEN
        X = Z(OUTPUT_TYPE,  I, JJ)
    ELSE IF (OUTPUT_TYPE == 3) THEN
        X = PP(I, JJ)
    ELSE
        X = TW(OUTPUT_TYPE, I, JJ)
    END IF
    
    ! DISPLAYING THE USER INPUTS

    PRINT  *,                               NEW_LINE('1'),                      &
                                            NEW_LINE('1')

    WRITE (*, '(A, 50("-"), A, A)')  ' ',   NEW_LINE('1'),                      &
                                          ' USER INPUTS:'
    WRITE (*, '(A, 50("-"))')  ' '

    WRITE (*, '(A, A)')                   ' INPUT TYPE            =  ',         &
                                            TRIM(INPUT_TYPES(INPUT_TYPE))
    WRITE (*, '(A, A)')                   ' OUTPUT TYPE           =  ',         &
                                            TRIM(ADJUSTL(OUTPUT_TYPES(OUTPUT_TYPE)))
    WRITE (*, '(A, A)')                   ' INPUT MONTH           =  ',         &
                                            TRIM(MONTHS_LIST(INPUT_MONTH))
    WRITE (*, OUTPUT_FORMAT)              ' INPUT LATITUDE        =  ',         &
                                            INPUT_LATITUDE

    IF (INPUT_TYPE == 1) THEN
        WRITE (*, OUTPUT_FORMAT)          ' INPUT PRESSURE VALUE  =  ',         &
                                            INPUT_PRESSURE
    ELSE IF (INPUT_TYPE == 2) THEN
        WRITE (*, OUTPUT_FORMAT)          ' INPUT HEIGHT VALUE    =  ',         &
                                            INPUT_HEIGHT
    END IF

    ! DISPLAYING THE OUTPUT RESULTS

    WRITE (*, '(A, A, 50("-"), A, A)')      NEW_LINE('1'),                      &
                                          ' ',                                  &
                                            NEW_LINE('1'),                      &
                                          ' OUTPUT RESULTS:'
    WRITE (*, '(A, 50("-"))')             ' '

    IF (INPUT_TYPE == 1) THEN

        WRITE (*, OUTPUT_FORMAT)          ' SCALE HEIGHT          =  ',         &
                                            SCALE_HEIGHT(I)
        WRITE (*, OUTPUT_FORMAT)          ' PRESSURE (MB)         =  ',         &
                                            PRESSURE(I)
        WRITE (*, OUTPUT_FORMAT)          ' GEOMETRIC HEIGHT      =  ',         &
                                            H(I)
        WRITE (*, OUTPUT_FORMAT)          ' OUTPUT VALUE          =  ',         &
                                            REAL(X, 16)

    ELSE IF (INPUT_TYPE == 2) THEN

        IF (OUTPUT_TYPE /= 3) THEN
            WRITE (*, OUTPUT_FORMAT)      ' HEIGHT (KM)           =  ',         &
                                            INVERSE_HEIGHT(I)
        ELSE
            WRITE (*, OUTPUT_FORMAT)      ' HEIGHT (KM)           =  ',         &
                                            INVERSE_HEIGHT(I),                  &
                                            IEX(I)
        END IF
        WRITE     (*, OUTPUT_FORMAT)      ' OUTPUT VALUE          =  ',         &
                                            REAL(X)

    END IF

    PRINT *
    PRINT *
    PRINT *

END PROGRAM CIRA86
