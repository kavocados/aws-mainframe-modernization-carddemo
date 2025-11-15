# CLAUDE.md - AI Assistant Guide for CardDemo Repository

> **Last Updated**: November 15, 2025
> **Repository**: aws-mainframe-modernization-carddemo
> **Version**: 1.0.0
> **License**: Apache 2.0

## Table of Contents
1. [Repository Overview](#repository-overview)
2. [Technology Stack](#technology-stack)
3. [Codebase Structure](#codebase-structure)
4. [File Naming Conventions](#file-naming-conventions)
5. [Development Workflows](#development-workflows)
6. [Key Architectural Patterns](#key-architectural-patterns)
7. [Working with COBOL Code](#working-with-cobol-code)
8. [Data Management](#data-management)
9. [Common Development Tasks](#common-development-tasks)
10. [Testing and Validation](#testing-and-validation)
11. [Git Workflow](#git-workflow)
12. [Important Constraints and Considerations](#important-constraints-and-considerations)

---

## Repository Overview

### Purpose
CardDemo is a comprehensive **mainframe credit card management application** designed specifically to test and showcase AWS and partner technologies for **mainframe migration and modernization scenarios**. This is NOT production code but rather a sophisticated test environment that intentionally incorporates various coding styles and patterns to exercise analysis, transformation, and migration tooling.

### Primary Use Cases
- Application discovery and analysis
- Migration assessment and planning
- Modernization strategy development
- Performance testing
- System augmentation
- Service enablement and extraction
- Test creation and automation

### Key Characteristics
- **42 COBOL programs** (~19,500 lines of code)
- **29 base programs** + **13 optional module programs**
- **17 BMS screen definitions** for online transactions
- **38 batch JCL jobs**
- **41 copybooks** for data structure consistency
- **Multiple integration patterns**: CICS online, batch processing, asynchronous messaging

---

## Technology Stack

### Core Technologies (Base Application)

| Technology | Purpose | Files/Components |
|------------|---------|------------------|
| **COBOL** | Primary programming language | 29 programs (base) |
| **CICS** | Online transaction processing | 17 BMS maps + transaction programs |
| **VSAM KSDS** | Keyed Sequential Data Sets (with AIX) | Primary data storage |
| **JCL** | Batch job control language | 38 batch jobs |
| **BMS** | Basic Mapping Support (screen definitions) | 17 mapsets |
| **Assembler** | Low-level utilities | MVSWAIT, COBDATFT |

### Optional Technologies (Extended Modules)

| Technology | Module | Purpose |
|------------|--------|---------|
| **DB2** | Transaction Type Management | Relational database with SQL operations |
| **IMS DB** | Credit Card Authorizations | Hierarchical database (HIDAM structure) |
| **IBM MQ** | Multiple modules | Asynchronous message queuing |
| **Control-M** | Scheduler | Job orchestration and scheduling |
| **CA7** | Scheduler | Alternative job scheduling |

### Data Formats and Structures
- **Fixed-format COBOL** (80-column punch card format)
- **EBCDIC and ASCII** data files
- **COMP, COMP-3** (packed decimal), Zoned Decimal
- **Signed/Unsigned** numeric fields
- **VSAM**: KSDS (keyed), ESDS (entry-sequenced), RRDS (relative record)
- **GDG** (Generation Data Groups)
- **Complex copybook structures**: REDEFINES, OCCURS, OCCURS DEPENDING ON

---

## Codebase Structure

### Directory Layout

```
/aws-mainframe-modernization-carddemo/
├── app/                          # Core application code (4.2 MB)
│   ├── cbl/                      # COBOL programs (29 files, 1.2 MB)
│   ├── cpy/                      # Copybooks (41 files, 143 KB)
│   ├── bms/                      # BMS screen definitions (17 files, 281 KB)
│   ├── cpy-bms/                  # BMS copybook maps (21 files, 227 KB)
│   ├── jcl/                      # JCL batch jobs (38 files, 159 KB)
│   ├── data/                     # Sample data files (593 KB)
│   │   ├── EBCDIC/              # Binary mainframe data format
│   │   └── ASCII/               # Text format for transfers
│   ├── catlg/                    # VSAM catalog files (196 KB)
│   ├── csd/                      # CICS System Definitions (34 KB)
│   ├── scheduler/                # CA7 and Control-M definitions (53 KB)
│   ├── proc/                     # Execution procedures (2 files, 13 KB)
│   ├── ctl/                      # DB2 control files (5.5 KB)
│   ├── asm/                      # Assembler utilities (2 files, 8 KB)
│   ├── maclib/                   # Macro libraries (2 files, 6.5 KB)
│   ├── app-authorization-ims-db2-mq/  # Optional Module 1 (583 KB)
│   ├── app-transaction-type-db2/      # Optional Module 2 (392 KB)
│   └── app-vsam-mq/                   # Optional Module 3 (111 KB)
├── diagrams/                     # Architecture and UI diagrams (12 PNG + 1 DrawIO)
├── samples/                      # Compilation templates and runtime samples
├── scripts/                      # Utility scripts for deployment
├── README.md                     # Comprehensive project documentation
├── CONTRIBUTING.md               # Contribution guidelines
├── LICENSE                       # Apache 2.0 license
└── CLAUDE.md                     # This file

```

### Key Application Modules

#### Base Application (29 Programs)

**Online CICS Transactions:**
- `COSGN00C` (CC00) - Sign-on/Login screen
- `COMEN01C` (CM00) - Main menu
- `COACTVWC` (CAVW) - Account view
- `COACTUPC` (CAUP) - Account update (4,236 lines - largest program)
- `COCRDLIC` (CCLI) - Credit card list (1,459 lines)
- `COCRDSLC` (CCDL) - Credit card detail view
- `COCRDUPC` (CCUP) - Credit card update (1,560 lines)
- `COTRN00C` (CT00) - Transaction list
- `COTRN01C` (CT01) - Transaction view
- `COTRN02C` (CT02) - Transaction add
- `CORPT00C` (CR00) - Transaction reports
- `COBIL00C` (CB00) - Bill payment
- `COUSR00C/01C/02C/03C` (CU00-03) - User management
- `COADM01C` (CA00) - Admin menu

**Batch Programs:**
- `CBTRN02C` - Transaction posting (core batch processing)
- `CBTRN03C` - Transaction reporting
- `CBACT04C` - Interest calculations (652 lines)
- `CBSTM03A` - Statement generation
- `CBEXPORT/CBIMPORT` - Data export/import with EBCDIC conversion
- `CBCUS01C` - Customer data processing
- `CSUTLDTC` - Utility functions (shared)

#### Optional Module 1: Credit Card Authorizations (IMS-DB2-MQ)
**Location**: `app/app-authorization-ims-db2-mq/`

- `COPAUA0C` (CP00) - MQ-triggered authorization processor
- `COPAUS0C` (CPVS) - Authorization summary viewer
- `COPAUS1C` (CPVD) - Authorization details viewer
- `COPAUS2C` - Fraud marking and DB2 updates
- `CBPAUP0C` - Batch purging of expired authorizations
- **Data Store**: IMS HIDAM (hierarchical) + DB2 fraud tracking
- **Integration**: Real-time via IBM MQ

#### Optional Module 2: Transaction Type Management (DB2)
**Location**: `app/app-transaction-type-db2/`

- `COTRTUPC` (CTTU) - Add/edit transaction types
- `COTRTLIC` (CTLI) - List/update/delete with DB2 cursors
- `COBTUPDT` - Batch maintenance
- **DB2 Tables**: TRANSACTION_TYPE, TRANSACTION_TYPE_CATEGORY
- **Pattern**: DB2 (admin) → sync to VSAM (operational high-performance access)

#### Optional Module 3: Account Extractions (MQ-VSAM)
**Location**: `app/app-vsam-mq/`

- `CODATE01` (CDRD) - System date inquiry via MQ
- `COACCT01` (CDRA) - Account details extraction via MQ
- **Pattern**: Asynchronous request/response with message correlation

---

## File Naming Conventions

### COBOL Programs
```
CO*.cbl     - CICS Online programs (e.g., COSGN00C.cbl, COACTUPC.cbl)
CB*.cbl     - Batch programs (e.g., CBTRN02C.cbl, CBACT04C.cbl)
CS*.cbl     - Shared/utility programs (e.g., CSUTLDTC.cbl)
```

### Copybooks
```
C*.cpy      - All copybooks start with 'C' (e.g., CSUSR01Y.cpy, CVACT01Y.cpy)
            - Typically end in 'Y' suffix (e.g., CVTRA05Y.cpy)
```

### BMS Screen Definitions
```
C*.bms      - BMS source files (e.g., COSGN00.bms, COCRDLI.bms)
C*.cpy      - Generated BMS copybooks in cpy-bms/ directory
```

### JCL Jobs
```
*.jcl       - Mixed case (e.g., ACCTFILE.jcl, POSTTRAN.jcl)
            - Descriptive names indicating purpose
```

### Data Files
```
*data.txt   - ASCII format (e.g., acctdata.txt, custdata.txt)
*.EBCDIC    - Binary EBCDIC format (e.g., acctdata.EBCDIC)
*.PS        - Sequential dataset suffix
*.VSAM      - VSAM dataset indicator
```

---

## Development Workflows

### Initial Setup and Initialization

When working with this repository for the first time, the proper initialization sequence is critical:

#### Step 1: Dataset Creation (Mainframe)
Create datasets with the specified HLQ (High Level Qualifier):
- `*.CARDDEMO.JCL` (FB 80)
- `*.CARDDEMO.CBL` (FB 80)
- `*.CARDDEMO.CPY` (FB 80)
- `*.CARDDEMO.BMS` (FB 80)
- `*.CARDDEMO.ASM` (FB 80)
- `*.CARDDEMO.PROC` (FB 80)
- `*.CARDDEMO.MACLIB` (FB 80)

#### Step 2: Upload Source Code
Transfer files from repository to mainframe in appropriate modes:
- **Text mode**: COBOL (.cbl), JCL (.jcl), copybooks (.cpy), BMS (.bms)
- **Binary mode**: Data files from `app/data/EBCDIC/`

#### Step 3: Execute Initialization JCLs (in order)
```
1. DUSRSECJ    - Load user security file
2. CLOSEFIL    - Close CICS files (if CICS is running)
3. ACCTFILE    - Load account master data
4. CARDFILE    - Load card master data
5. CUSTFILE    - Create customer database
6. XREFFILE    - Load cross-reference data
7. TRANFILE    - Load initial transaction file
8. DISCGRP     - Load disclosure groups
9. TCATBALF    - Load transaction category balance
10. TRANCATG   - Load transaction categories
11. TRANTYPE   - Load transaction types
12. DEFGDGB    - Define GDG bases
13. DEFGDGD    - Define additional GDG bases (for DB2 module)
14. OPENFIL    - Open files for CICS
```

**Optional Module Initialization:**
- `CREADB21` - Create DB2 database (Transaction Type Management)
- `TRANEXTR` - Extract DB2 data to VSAM (Transaction Type Management)

#### Step 4: Compile Programs
Use sample compilation JCLs from `samples/` directory:
- `BATCMP` - Batch COBOL compilation template
- `CICCMP` - CICS COBOL compilation template
- `BMSCMP` - BMS compilation template
- `CICDBCMP` - CICS with DB2 compilation template
- `IMSMQCMP` - IMS with MQ compilation template

#### Step 5: Configure CICS Resources
- **Preferred method**: Use DFHCSDUP utility with CSD files
- **Alternative**: Manual CEDA commands to define programs, transactions, files, mapsets

### Daily Batch Processing Cycle

The application follows a standard daily batch workflow:

```
CLOSEFIL (close VSAM files)
    ↓
TRANBKP (backup transaction data)
    ↓
POSTTRAN (post transactions - CORE JOB)
    ↓
INTCALC (calculate interest)
    ↓
COMBTRAN (combine transaction files)
    ↓
CREASTMT (create statements)
    ↓
OPENFIL (reopen files for CICS)
```

**Parallel processing available** after CLOSEFIL:
- DISCGRP (disclosure groups refresh)
- Transaction processing chains

### Scheduler Integration

Jobs can be orchestrated using:
- **Control-M**: `app/scheduler/CardDemo.controlm` (XML definition)
- **CA7**: `app/scheduler/CardDemo.ca7` (scheduler file)

**Key scheduled workflows:**
- Daily transaction backup chain
- Weekly transaction type DB2 refresh
- Disclosure groups refresh

---

## Key Architectural Patterns

### 1. Layered Architecture
```
Presentation Layer (BMS Screens)
    ↓
Transaction Layer (CICS Programs)
    ↓
Business Logic Layer (COBOL Program Logic)
    ↓
Data Access Layer (VSAM/DB2/IMS File I/O)
    ↓
Data Storage (VSAM/DB2/IMS Databases)
```

### 2. Online vs. Batch Processing

**Online (CICS) Characteristics:**
- Interactive user transactions
- BMS screen I/O
- Direct VSAM reads for performance
- Real-time validation
- Transaction rollback capabilities

**Batch Characteristics:**
- Sequential file processing
- High-volume data manipulation
- Report generation
- Database reorganization
- Backup and recovery operations

### 3. Integration Patterns

**Synchronous Pattern (CICS → VSAM)**
```cobol
EXEC CICS READ
    FILE('ACCTFILE')
    INTO(ACCOUNT-RECORD)
    RIDFLD(ACCOUNT-ID)
END-EXEC
```

**Asynchronous Pattern (MQ Request/Response)**
```cobol
* Put request message
CALL 'MQPUT' USING ...
* Get response message with correlation ID
CALL 'MQGET' USING ...
```

**Database Integration (DB2)**
```cobol
EXEC SQL
    SELECT TR_TYPE, TR_DESCRIPTION
    INTO :WS-TR-TYPE, :WS-TR-DESC
    FROM TRANSACTION_TYPE
    WHERE TR_TYPE = :WS-INPUT-TYPE
END-EXEC
```

### 4. Data Consistency Pattern

The application uses **copybooks** extensively to ensure data structure consistency across programs:

```
CVACT01Y.cpy (Account structure)
    ↓ COPY into
    ├── COACTVWC.cbl (View)
    ├── COACTUPC.cbl (Update)
    ├── CBACT04C.cbl (Batch interest calculation)
    └── Multiple other programs
```

### 5. Dual-Storage Pattern (DB2 Module)

For reference data that changes infrequently:
1. **DB2 Tables**: Authoritative source, updated via admin screens
2. **VSAM Files**: Synchronized copy for high-performance operational access
3. **TRANEXTR Job**: Periodic synchronization from DB2 → VSAM

---

## Working with COBOL Code

### Standard COBOL Program Structure

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMNAME.
       AUTHOR. [Author info].
      *****************************************************************
      * Program description and history
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-NAME ...

       DATA DIVISION.
       FILE SECTION.
       FD  FILE-NAME.
       01  FILE-RECORD.

       WORKING-STORAGE SECTION.
      * Copybooks
       COPY COPYBOOK1.
       COPY COPYBOOK2.
      * Working storage variables
       01  WS-VARIABLE-NAME.

       LINKAGE SECTION.
      * For subprograms

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZATION
           PERFORM 2000-PROCESS-DATA
           PERFORM 9000-CLEANUP
           STOP RUN.
```

### CICS Program Patterns

**Standard CICS Transaction Flow:**
```cobol
      * Receive map from terminal
       EXEC CICS RECEIVE
           MAP('mapname')
           MAPSET('mapset')
           INTO(map-area)
       END-EXEC

      * Process data
       PERFORM 2000-PROCESS-BUSINESS-LOGIC

      * Send map to terminal
       EXEC CICS SEND
           MAP('mapname')
           MAPSET('mapset')
           FROM(map-area)
       END-EXEC

       EXEC CICS RETURN
           TRANSID('XXXX')
           COMMAREA(communication-area)
       END-EXEC
```

**VSAM File Access in CICS:**
```cobol
      * Random read
       EXEC CICS READ
           FILE('FILENAME')
           INTO(record-area)
           RIDFLD(key-field)
           RESP(ws-response)
       END-EXEC

      * Update
       EXEC CICS REWRITE
           FILE('FILENAME')
           FROM(record-area)
       END-EXEC
```

### Common COBOL Constructs in This Codebase

**1. REDEFINES (Multiple interpretations of same data)**
```cobol
       01  TRANSACTION-AMOUNT.
           05  TRAN-AMT-NUMERIC    PIC 9(9)V99.
       01  TRAN-AMT-DISPLAY REDEFINES TRANSACTION-AMOUNT.
           05  TRAN-DOLLARS        PIC 9(9).
           05  TRAN-CENTS          PIC 9(2).
```

**2. OCCURS (Arrays/Tables)**
```cobol
       01  TRANSACTION-TABLE.
           05  TRANSACTION-ENTRY OCCURS 100 TIMES.
               10  TRAN-ID         PIC X(16).
               10  TRAN-DATE       PIC X(10).
               10  TRAN-AMOUNT     PIC 9(9)V99.
```

**3. OCCURS DEPENDING ON (Variable-length arrays)**
```cobol
       01  CUSTOMER-RECORD.
           05  NUM-TRANSACTIONS    PIC 999.
           05  TRANS-DETAIL OCCURS 0 TO 999 TIMES
               DEPENDING ON NUM-TRANSACTIONS.
               10  TRANS-ID        PIC X(16).
```

**4. COMP and COMP-3 (Binary and Packed Decimal)**
```cobol
       01  NUMERIC-FIELDS.
           05  BINARY-COUNTER      PIC 9(9) COMP.
           05  PACKED-AMOUNT       PIC 9(9)V99 COMP-3.
```

### Important COBOL Best Practices for AI Assistants

1. **Preserve Column Formatting**: COBOL is column-sensitive (1-6: sequence, 7: indicator, 8-72: code)
2. **Maintain Indentation**: Use consistent spacing for readability
3. **Respect 80-Column Limit**: Traditional mainframe constraint
4. **Use Existing Copybooks**: Don't duplicate data structures
5. **Follow Naming Conventions**: Program prefixes (CO/CB/CS), meaningful names
6. **Comment Changes**: Use inline comments for modifications
7. **Test Impact**: Changes can affect multiple programs sharing copybooks

---

## Data Management

### VSAM Datasets

**Primary VSAM Files:**

| File Name | Type | Purpose | Copybook | Record Length |
|-----------|------|---------|----------|---------------|
| ACCTFILE | KSDS | Account master | CVACT01Y | 300 |
| CARDFILE | KSDS | Credit card master | CVACT02Y | 150 |
| CUSTFILE | KSDS | Customer master | CVCUS01Y | 500 |
| CARDXREF | KSDS | Customer-account-card xref | CVACT03Y | 50 |
| TRANSACT | KSDS | Online transaction data | CVTRA05Y | 350 |
| USRSEC | KSDS | User security | CSUSR01Y | 80 |
| DALYTRAN | Sequential | Daily transaction input | CVTRA06Y | 350 |
| DISCGRP | KSDS | Disclosure groups | CVTRA02Y | 50 |
| TRANCATG | KSDS | Transaction categories | CVTRA04Y | 60 |
| TRANTYPE | KSDS | Transaction types | CVTRA03Y | 60 |
| TCATBALF | KSDS | Category balance | CVTRA01Y | 50 |

**Alternate Indexes (AIX):**
- Transaction file by date
- Card file by account number
- Customer cross-references

### Data Formats

**EBCDIC vs. ASCII:**
- **EBCDIC files**: Binary mainframe format, in `app/data/EBCDIC/`
- **ASCII files**: Text format for easier transfer, in `app/data/ASCII/`
- **Conversion programs**: CBEXPORT (EBCDIC→ASCII), CBIMPORT (ASCII→EBCDIC)

**Numeric Data Representation:**
```
DISPLAY:      PIC 9(5)        - 5 bytes, human-readable
COMP (BINARY): PIC 9(5) COMP  - 2-4 bytes, binary integer
COMP-3 (PACKED): PIC 9(5) COMP-3 - 3 bytes, packed decimal
ZONED DECIMAL: PIC S9(5)      - 5 bytes, signed
```

### Sample Data Sets

**Test Data Included:**
- 100 customer records
- 100 account records
- 50 credit card records
- 300+ transaction records
- Reference data (transaction types, categories)

**Default Login Credentials:**
- Admin: `ADMIN001` / `PASSWORD`
- User: `USER0001` / `PASSWORD`

---

## Common Development Tasks

### Task 1: Adding a New CICS Transaction Screen

**Steps:**
1. Create BMS source file in `app/bms/` (e.g., `CONEW01.bms`)
2. Create COBOL program in `app/cbl/` (e.g., `CONEW01C.cbl`)
3. Copy existing copybook or create new one if needed
4. Compile BMS using BMSCMP template
5. Compile COBOL using CICCMP template
6. Update CICS CSD file in `app/csd/CARDDEMO.CSD`:
   ```
   DEFINE PROGRAM(CONEW01C) GROUP(CARDDEMO) ...
   DEFINE MAPSET(CONEW01) GROUP(CARDDEMO) ...
   DEFINE TRANSACTION(CN01) GROUP(CARDDEMO) PROGRAM(CONEW01C) ...
   ```
7. Install in CICS:
   ```
   CEDA INSTALL PROGRAM(CONEW01C) GROUP(CARDDEMO)
   CEDA INSTALL MAPSET(CONEW01) GROUP(CARDDEMO)
   CEDA INSTALL TRANSACTION(CN01) GROUP(CARDDEMO)
   ```

### Task 2: Adding a New Batch Job

**Steps:**
1. Create COBOL program in `app/cbl/` with CB* prefix (e.g., `CBNEW01C.cbl`)
2. Create or reference copybooks for data structures
3. Create JCL job in `app/jcl/` (e.g., `NEWBATCH.jcl`)
4. Define file allocations (DD statements) in JCL
5. Compile using BATCMP template
6. Test with sample data
7. Integrate into batch cycle if needed (update scheduler files)

### Task 3: Modifying a Copybook

**Critical Process:**
1. **Identify all impacted programs** using the copybook:
   ```bash
   grep -r "COPY CVACT01Y" app/cbl/
   ```
2. **Assess impact**: Does change affect record length, key fields, or data types?
3. **Update copybook** in `app/cpy/`
4. **Recompile all affected programs**
5. **Update VSAM file definitions** if record length changed
6. **Update sample data** in `app/data/` to match new structure
7. **Test thoroughly**: Impact cascades through entire application

### Task 4: Adding Optional Module Integration

**Example: Integrating MQ Support**
1. Review existing optional module structure (e.g., `app/app-vsam-mq/`)
2. Create module directory structure:
   ```
   app/app-new-feature/
   ├── cbl/
   ├── cpy/
   ├── jcl/
   ├── csd/
   └── README.md
   ```
3. Develop programs following naming conventions
4. Create separate CSD file for module resources
5. Document prerequisites and setup in module README
6. Update main README.md with optional feature description

### Task 5: Updating Scheduler Definitions

**Control-M:**
1. Edit `app/scheduler/CardDemo.controlm` (XML format)
2. Define job dependencies using INCOND/OUTCOND
3. Set scheduling criteria (days, time)
4. Test job flow logic

**CA7:**
1. Edit `app/scheduler/CardDemo.ca7`
2. Define job dependencies and predecessors
3. Configure restart/rerun capabilities
4. Set critical job flags

---

## Testing and Validation

### Pre-Deployment Checklist

**Before committing COBOL changes:**
- [ ] Code compiles without errors or warnings
- [ ] All copybooks referenced are present
- [ ] Column formatting is correct (columns 1-6, 7, 8-72)
- [ ] Naming conventions followed (CO*/CB*/CS* prefix)
- [ ] Comments added for significant changes

**Before committing JCL changes:**
- [ ] Dataset names use correct HLQ pattern
- [ ] DD statements reference existing files
- [ ] EXEC statements reference compiled programs
- [ ] Job dependencies are correct
- [ ] SYSOUT classes are appropriate

**Before committing data changes:**
- [ ] EBCDIC files match ASCII counterparts (if both maintained)
- [ ] Record lengths match copybook definitions
- [ ] Key fields are properly populated
- [ ] Test data covers edge cases

### Testing Approach

**Unit Testing:**
- Test individual COBOL programs in isolation
- Use test VSAM files (not production data)
- Validate input/output with sample copybook structures

**Integration Testing:**
- Test full transaction flows (online)
- Test batch job sequences
- Verify VSAM file updates
- Check DB2/IMS integration (if optional modules used)

**End-to-End Testing:**
- Execute complete daily batch cycle
- Test all online screens in sequence
- Verify reports and statements generated
- Check scheduler job execution

---

## Git Workflow

### Branch Strategy

**Current Development Branch:**
```
claude/claude-md-mi09inklkdys48m8-01WrWUBkt4VqVgeNKp4rD3DX
```

**Important Rules:**
1. Always develop on branches starting with `claude/`
2. Branch names must end with matching session ID
3. Push failures occur if branch naming is incorrect (403 error)
4. Main branch is used for pull requests

### Commit Best Practices

**Commit Message Format:**
```
Brief description (50 chars or less)

More detailed explanation if needed:
- What was changed
- Why it was changed
- Any impact on other components

Affected files:
- app/cbl/PROGRAM.cbl
- app/cpy/COPYBOOK.cpy
```

**Atomic Commits:**
- One logical change per commit
- Don't mix unrelated changes
- Include all affected files (program + copybook if both changed)

### Push Operations

**Standard push command:**
```bash
git push -u origin claude/claude-md-mi09inklkdys48m8-01WrWUBkt4VqVgeNKp4rD3DX
```

**Retry logic for network failures:**
- Retry up to 4 times with exponential backoff (2s, 4s, 8s, 16s)
- Only retry for network errors, not authentication failures

---

## Important Constraints and Considerations

### For AI Assistants Working with This Code

#### 1. This is a Mainframe Modernization Test Environment
- **Not production code**: Designed to test migration tools and strategies
- **Intentionally diverse**: Multiple coding styles and patterns are by design
- **Educational purpose**: Showcases various mainframe technologies

#### 2. Preserve Mainframe Characteristics
- **Don't "modernize" the code style**: The COBOL should remain authentic mainframe code
- **Keep 80-column format**: This is a deliberate constraint
- **Maintain EBCDIC compatibility**: Data files must remain compatible
- **Respect fixed-format COBOL**: Columns 1-6, 7, 8-72 have specific meanings

#### 3. Data Integrity is Critical
- **Copybooks are contracts**: Changes cascade through many programs
- **VSAM record lengths must match**: Mismatches cause runtime failures
- **Key field changes break indexes**: Alternate indexes rely on specific fields
- **EBCDIC encoding matters**: Binary data transfers require special handling

#### 4. Dependencies are Complex
- **Program compilation order matters**: Some programs depend on others
- **JCL execution order is strict**: Batch jobs must run in sequence
- **CICS resources must be defined**: Programs/transactions won't work without CSD entries
- **Scheduler dependencies**: Jobs have predecessor/successor relationships

#### 5. Technology-Specific Constraints

**COBOL:**
- Maximum 30-character variable names
- Case-insensitive language (conventionally uppercase)
- Period (.) terminates statements
- COPY statement must reference existing copybooks

**CICS:**
- Transaction IDs are 4 characters (e.g., CC00, CAVW)
- Programs must be defined before transactions
- COMMAREA limited to 32KB
- Screen map names often match program names

**VSAM:**
- Record keys must be unique in KSDS
- Alternate indexes require base cluster to exist
- File must be closed before reorganization
- SHAREOPTIONS affect concurrent access

**JCL:**
- Maximum 8-character job names
- DD names limited to 8 characters
- Continuation requires specific column placement
- STEPLIB/JOBLIB order affects program resolution

#### 6. Security and Credentials
- **Default credentials are public**: ADMIN001/PASSWORD is for testing only
- **RACF integration**: Security definitions would be needed in real mainframe
- **User file contains hashed passwords**: Don't commit plaintext credentials

#### 7. Optional Modules
- **Can be installed independently**: Don't assume all modules are present
- **Have additional prerequisites**: DB2, IMS, MQ may not be available
- **Documented separately**: Each module has its own README

#### 8. Documentation Standards
- **Update README.md**: Significant changes should be documented
- **Module-specific READMEs**: Optional features documented in module directories
- **Inline comments**: COBOL code should have clear comments
- **Diagram updates**: Screen/flow changes may require diagram updates

#### 9. Performance Considerations
- **VSAM is faster than DB2**: This is why dual-storage pattern exists
- **Batch processing is optimized for volume**: Don't add interactive features to batch
- **CICS transactions should be fast**: Minimize wait times and file I/O
- **AIX impacts update performance**: Alternate indexes slow down writes

#### 10. Common Pitfalls to Avoid
- ❌ Don't change copybook record lengths without updating all VSAM definitions
- ❌ Don't add DB2 code to programs without updating compilation JCL
- ❌ Don't modify key fields in VSAM records
- ❌ Don't change COBOL variable names without searching all references
- ❌ Don't assume case-sensitivity (COBOL is case-insensitive)
- ❌ Don't add Unicode characters (mainframe uses EBCDIC)
- ❌ Don't create JCL with Linux line endings (use CRLF or mainframe format)
- ❌ Don't skip initialization JCLs when setting up
- ❌ Don't commit EBCDIC files without corresponding ASCII versions
- ❌ Don't modify scheduler files without understanding job dependencies

---

## Quick Reference

### Most Frequently Modified Files

**Adding new functionality:**
- `app/cbl/*.cbl` - COBOL programs
- `app/cpy/*.cpy` - Data structures
- `app/jcl/*.jcl` - Batch jobs
- `app/csd/CARDDEMO.CSD` - CICS resource definitions

**Configuration changes:**
- `app/scheduler/CardDemo.controlm` - Control-M jobs
- `app/scheduler/CardDemo.ca7` - CA7 jobs
- `app/csd/*.csd` - CICS system definitions

**Sample data updates:**
- `app/data/ASCII/*.txt` - Text format data
- `app/data/EBCDIC/*` - Binary format data

### Key Entry Points

**Starting the application:**
- Transaction: `CC00` (Signon)
- Main menu: `CM00`
- Admin menu: `CA00`

**Core batch jobs:**
- `POSTTRAN.jcl` - Transaction posting (core processing)
- `INTCALC.jcl` - Interest calculations
- `CLOSEFIL.jcl` / `OPENFIL.jcl` - CICS file management

**Largest/Most Complex Programs:**
- `COACTUPC.cbl` (4,236 lines) - Account update
- `COCRDUPC.cbl` (1,560 lines) - Credit card update
- `COCRDLIC.cbl` (1,459 lines) - Credit card listing

### Useful Commands

**Search for copybook usage:**
```bash
grep -r "COPY COPYNAME" app/cbl/
```

**Find all programs using a transaction code:**
```bash
grep -r "TRANSID('CT00')" app/cbl/
```

**List all JCL jobs:**
```bash
ls -1 app/jcl/*.jcl
```

**Find programs accessing a specific file:**
```bash
grep -r "FILE('ACCTFILE')" app/cbl/
```

---

## Resources and References

### Internal Documentation
- [Main README](./README.md) - Comprehensive project overview
- [Contributing Guidelines](./CONTRIBUTING.md) - How to contribute
- [IMS-DB2-MQ Module](./app/app-authorization-ims-db2-mq/README.md) - Authorization feature
- [Transaction Type DB2 Module](./app/app-transaction-type-db2/README.md) - DB2 integration
- [MQ-VSAM Module](./app/app-vsam-mq/README.md) - MQ integration
- [Architecture Diagrams](./diagrams/) - Visual representations

### External Resources
- COBOL Language Reference
- CICS Application Programming Guide
- DB2 SQL Reference
- IMS Database Administration Guide
- IBM MQ Programming Guide
- JCL Reference Manual
- VSAM Administration Guide

### AWS Mainframe Modernization
- AWS Mainframe Modernization Service Documentation
- Migration Pattern Libraries
- Modernization Assessment Tools

---

## Changelog

### November 15, 2025
- Initial creation of CLAUDE.md
- Comprehensive analysis of repository structure
- Documentation of development workflows and conventions
- AI assistant guidelines established

---

## Contact and Support

For questions or issues:
1. Review this CLAUDE.md file
2. Check the main [README.md](./README.md)
3. Review module-specific documentation in respective directories
4. Raise an issue in the GitHub repository with detailed information

**License**: Apache 2.0
**Maintainer**: AWS and Community Contributors
**Purpose**: Mainframe Modernization Testing and Education

---

**Note for AI Assistants**: This is a sophisticated mainframe application with complex interdependencies. Always consider the impact of changes across multiple files, maintain authentic mainframe coding practices, and test thoroughly. When in doubt, preserve existing patterns and consult the comprehensive documentation provided.
