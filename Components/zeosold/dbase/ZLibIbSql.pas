{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{             Delphi interface to gds32.dll              }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZLibIbSql;

interface

uses {$IFNDEF LINUX}Windows{$ELSE}Types{$ENDIF}, Classes, ZSqlTypes;

{***************** Plain API Constants definition ****************}

const
{$IFNDEF LINUX}
  DEFAULT_DLL_LOCATION   = 'gds32.dll';
{$ELSE}
  DEFAULT_DLL_LOCATION   = '/usr/lib/libgds.so';
{$ENDIF}

const
  ISC_NULL = -1;
  ISC_NOTNULL = 0;

  ISC_TRUE                      = 1;
  ISC_FALSE                     = 0;
  DSQL_CLOSE                    = 1;
  DSQL_DROP                     = 2;

  SQLDA_VERSION1                = 1;
  SQLDA_VERSION2                = 2;
  SQL_DIALECT_V5                = 1;
  SQL_DIALECT_V6                = 2;
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; (* latest IB DIALECT *)

  { Actions to pass to the blob filter (ctl_source) }
  isc_blob_filter_open           = 0;
  isc_blob_filter_get_segment    = 1;
  isc_blob_filter_close          = 2;
  isc_blob_filter_create         = 3;
  isc_blob_filter_put_segment    = 4;
  isc_blob_filter_alloc          = 5;
  isc_blob_filter_free           = 6;
  isc_blob_filter_seek           = 7;

  { Database parameter block stuff }
  isc_dpb_version1               = 1;
  isc_dpb_cdd_pathname           = 1;
  isc_dpb_allocation             = 2;
  isc_dpb_journal                = 3;
  isc_dpb_page_size              = 4;
  isc_dpb_num_buffers            = 5;
  isc_dpb_buffer_length          = 6;
  isc_dpb_debug                  = 7;
  isc_dpb_garbage_collect        = 8;
  isc_dpb_verify                 = 9;
  isc_dpb_sweep                  = 10;
  isc_dpb_enable_journal         = 11;
  isc_dpb_disable_journal        = 12;
  isc_dpb_dbkey_scope            = 13;
  isc_dpb_number_of_users        = 14;
  isc_dpb_trace                  = 15;
  isc_dpb_no_garbage_collect     = 16;
  isc_dpb_damaged                = 17;
  isc_dpb_license                = 18;
  isc_dpb_sys_user_name          = 19;
  isc_dpb_encrypt_key            = 20;
  isc_dpb_activate_shadow        = 21;
  isc_dpb_sweep_interval         = 22;
  isc_dpb_delete_shadow          = 23;
  isc_dpb_force_write            = 24;
  isc_dpb_begin_log              = 25;
  isc_dpb_quit_log               = 26;
  isc_dpb_no_reserve             = 27;
  isc_dpb_user_name              = 28;
  isc_dpb_password               = 29;
  isc_dpb_password_enc           = 30;
  isc_dpb_sys_user_name_enc      = 31;
  isc_dpb_interp                 = 32;
  isc_dpb_online_dump            = 33;
  isc_dpb_old_file_size          = 34;
  isc_dpb_old_num_files          = 35;
  isc_dpb_old_file               = 36;
  isc_dpb_old_start_page         = 37;
  isc_dpb_old_start_seqno        = 38;
  isc_dpb_old_start_file         = 39;
  isc_dpb_drop_walfile           = 40;
  isc_dpb_old_dump_id            = 41;
  isc_dpb_wal_backup_dir         = 42;
  isc_dpb_wal_chkptlen           = 43;
  isc_dpb_wal_numbufs            = 44;
  isc_dpb_wal_bufsize            = 45;
  isc_dpb_wal_grp_cmt_wait       = 46;
  isc_dpb_lc_messages            = 47;
  isc_dpb_lc_ctype               = 48;
  isc_dpb_cache_manager          = 49;
  isc_dpb_shutdown               = 50;
  isc_dpb_online                 = 51;
  isc_dpb_shutdown_delay         = 52;
  isc_dpb_reserved               = 53;
  isc_dpb_overwrite              = 54;
  isc_dpb_sec_attach             = 55;
  isc_dpb_disable_wal            = 56;
  isc_dpb_connect_timeout        = 57;
  isc_dpb_dummy_packet_interval  = 58;
  isc_dpb_gbak_attach            = 59;
  isc_dpb_sql_role_name          = 60;
  isc_dpb_set_page_buffers       = 61;
  isc_dpb_working_directory      = 62;
  isc_dpb_SQL_dialect            = 63;
  isc_dpb_set_db_readonly        = 64;
  isc_dpb_set_db_SQL_dialect     = 65;
  isc_dpb_gfix_attach            = 66;
  isc_dpb_gstat_attach           = 67;
  isc_dpb_last_dpb_constant      = isc_dpb_gstat_attach;

  { isc_dpb_verify specific flags }
  isc_dpb_pages                  = 1;
  isc_dpb_records                = 2;
  isc_dpb_indices                = 4;
  isc_dpb_transactions           = 8;
  isc_dpb_no_update              = 16;
  isc_dpb_repair                 = 32;
  isc_dpb_ignore                 = 64;

  { isc_dpb_shutdown specific flags }
  isc_dpb_shut_cache             = 1;
  isc_dpb_shut_attachment        = 2;
  isc_dpb_shut_transaction       = 4;
  isc_dpb_shut_force             = 8;

  { Transaction parameter block stuff }
  isc_tpb_version1               = 1;
  isc_tpb_version3               = 3;
  isc_tpb_consistency            = 1;
  isc_tpb_concurrency            = 2;
  isc_tpb_shared                 = 3;
  isc_tpb_protected              = 4;
  isc_tpb_exclusive              = 5;
  isc_tpb_wait                   = 6;
  isc_tpb_nowait                 = 7;
  isc_tpb_read                   = 8;
  isc_tpb_write                  = 9;
  isc_tpb_lock_read              = 10;
  isc_tpb_lock_write             = 11;
  isc_tpb_verb_time              = 12;
  isc_tpb_commit_time            = 13;
  isc_tpb_ignore_limbo           = 14;
  isc_tpb_read_committed         = 15;
  isc_tpb_autocommit             = 16;
  isc_tpb_rec_version            = 17;
  isc_tpb_no_rec_version         = 18;
  isc_tpb_restart_requests       = 19;
  isc_tpb_no_auto_undo           = 20;
  isc_tpb_last_tpb_constant      = isc_tpb_no_auto_undo;

  { Blob Parameter Block }
  isc_bpb_version1               = 1;
  isc_bpb_source_type            = 1;
  isc_bpb_target_type            = 2;
  isc_bpb_type                   = 3;
  isc_bpb_source_interp          = 4;
  isc_bpb_target_interp          = 5;
  isc_bpb_filter_parameter       = 6;

  { SQL information items }
  isc_info_sql_select            = 4;
  isc_info_sql_bind              = 5;
  isc_info_sql_num_variables     = 6;
  isc_info_sql_describe_vars     = 7;
  isc_info_sql_describe_end      = 8;
  isc_info_sql_sqlda_seq         = 9;
  isc_info_sql_message_seq       = 10;
  isc_info_sql_type              = 11;
  isc_info_sql_sub_type          = 12;
  isc_info_sql_scale             = 13;
  isc_info_sql_length            = 14;
  isc_info_sql_null_ind          = 15;
  isc_info_sql_field             = 16;
  isc_info_sql_relation          = 17;
  isc_info_sql_owner             = 18;
  isc_info_sql_alias             = 19;
  isc_info_sql_sqlda_start       = 20;
  isc_info_sql_stmt_type         = 21;
  isc_info_sql_get_plan          = 22;
  isc_info_sql_records           = 23;
  isc_info_sql_batch_fetch       = 24;

  { SQL information return values }
  isc_info_sql_stmt_select         = 1;
  isc_info_sql_stmt_insert         = 2;
  isc_info_sql_stmt_update         = 3;
  isc_info_sql_stmt_delete         = 4;
  isc_info_sql_stmt_ddl            = 5;
  isc_info_sql_stmt_get_segment    = 6;
  isc_info_sql_stmt_put_segment    = 7;
  isc_info_sql_stmt_exec_procedure = 8;
  isc_info_sql_stmt_start_trans    = 9;
  isc_info_sql_stmt_commit         = 10;
  isc_info_sql_stmt_rollback       = 11;
  isc_info_sql_stmt_select_for_upd = 12;
  isc_info_sql_stmt_set_generator  = 13;

  isc_bpb_type_segmented           = 0;
  isc_bpb_type_stream              = 1;

  {************** Information call declarations **************}

  { Common, structural codes }
  isc_info_end                     = 1;
  isc_info_truncated               = 2;
  isc_info_error                   = 3;
  isc_info_data_not_ready          = 4;
  isc_info_flag_end                = 127;

  { Request information items }
  isc_info_number_messages         = 4;
  isc_info_max_message             = 5;
  isc_info_max_send                = 6;
  isc_info_max_receive             = 7;
  isc_info_state                   = 8;
  isc_info_message_number          = 9;
  isc_info_message_size            = 10;
  isc_info_request_cost            = 11;
  isc_info_access_path             = 12;
  isc_info_req_select_count        = 13;
  isc_info_req_insert_count        = 14;
  isc_info_req_update_count        = 15;
  isc_info_req_delete_count        = 16;

  { SQL definitions }
  SQL_VARYING                    = 448;
  SQL_TEXT                       = 452;
  SQL_DOUBLE                     = 480;
  SQL_FLOAT                      = 482;
  SQL_LONG                       = 496;
  SQL_SHORT                      = 500;
  SQL_TIMESTAMP                  = 510;
  SQL_BLOB                       = 520;
  SQL_D_FLOAT                    = 530;
  SQL_ARRAY                      = 540;
  SQL_QUAD                       = 550;
  SQL_TYPE_TIME                  = 560;
  SQL_TYPE_DATE                  = 570;
  SQL_INT64                      = 580;
  SQL_DATE                       = SQL_TIMESTAMP;

  { Blob Subtypes }
  { types less than zero are reserved for customer use }
  isc_blob_untyped               = 0;

  { internal subtypes }
  isc_blob_text                  = 1;
  isc_blob_blr                   = 2;
  isc_blob_acl                   = 3;
  isc_blob_ranges                = 4;
  isc_blob_summary               = 5;
  isc_blob_format                = 6;
  isc_blob_tra                   = 7;
  isc_blob_extfile               = 8;

  { the range 20-30 is reserved for dBASE and Paradox types }
  isc_blob_formatted_memo        = 20;
  isc_blob_paradox_ole           = 21;
  isc_blob_graphic               = 22;
  isc_blob_dbase_ole             = 23;
  isc_blob_typed_binary          = 24;

  { Blr definitions }
  blr_text = 14;
  blr_text2 = 15;
  blr_short = 7;
  blr_long = 8;
  blr_quad = 9;
  blr_float = 10;
  blr_double = 27;
  blr_d_float = 11;
  blr_timestamp = 35;
  blr_varying = 37;
  blr_varying2 = 38;
  blr_blob = 261;
  blr_cstring = 40;
  blr_cstring2 = 41;
  blr_blob_id = 45;
  blr_sql_date = 12;
  blr_sql_time = 13;
  blr_int64 = 16;
  blr_date = blr_timestamp;

  blr_inner = 0;
  blr_left = 1;
  blr_right = 2;
  blr_full = 3;

  blr_gds_code = 0;
  blr_sql_code = 1;
  blr_exception = 2;
  blr_trigger_code = 3;
  blr_default_code = 4;

  blr_version4 = 4;
  blr_version5 = 5;
  blr_eoc = 76;
  blr_end = -1;
 
  blr_assignment = 1;
  blr_begin = 2;
  blr_dcl_variable = 3;
  blr_message = 4;
  blr_erase = 5;
  blr_fetch = 6;
  blr_for = 7;
  blr_if = 8;
  blr_loop = 9;
  blr_modify = 10;
  blr_handler = 11;
  blr_receive = 12;
  blr_select = 13;
  blr_send = 14;
  blr_store = 15;
  blr_label = 17;
  blr_leave = 18;
  blr_store2 = 19;
  blr_post = 20;

  blr_literal = 21;
  blr_dbkey = 22;
  blr_field = 23;
  blr_fid = 24;
  blr_parameter = 25;
  blr_variable = 26;
  blr_average = 27;
  blr_count = 28;
  blr_maximum = 29;
  blr_minimum = 30;
  blr_total = 31;
  blr_add = 34;
  blr_subtract = 35;
  blr_multiply = 36;
  blr_divide = 37;
  blr_negate = 38;
  blr_concatenate = 39;
  blr_substring = 40;
  blr_parameter2 = 41;
  blr_from = 42;
  blr_via = 43;
  blr_user_name = 44;
  blr_null = 45;
 
  blr_eql = 47;
  blr_neq = 48;
  blr_gtr = 49;
  blr_geq = 50;
  blr_lss = 51;
  blr_leq = 52;
  blr_containing = 53;
  blr_matching = 54;
  blr_starting = 55;
  blr_between = 56;
  blr_or = 57;
  blr_and = 58;
  blr_not = 59;
  blr_any = 60;
  blr_missing = 61;
  blr_unique = 62;
  blr_like = 63;
 
  blr_stream = 65;
  blr_set_index = 66;
  blr_rse = 67;
  blr_first = 68;
  blr_project = 69;
  blr_sort = 70;
  blr_boolean = 71;
  blr_ascending = 72;
  blr_descending = 73;
  blr_relation = 74;
  blr_rid = 75;
  blr_union = 76;
  blr_map = 77;
  blr_group_by = 78;
  blr_aggregate = 79;
  blr_join_type = 80;

  blr_agg_count = 83;
  blr_agg_max = 84;
  blr_agg_min = 85;
  blr_agg_total = 86;
  blr_agg_average = 87;
  blr_parameter3 = 88;
  blr_run_count = 118;
  blr_run_max = 89;
  blr_run_min = 90;
  blr_run_total = 91;
  blr_run_average = 92;
  blr_agg_count2 = 93;
  blr_agg_count_distinct = 94;
  blr_agg_total_distinct = 95;
  blr_agg_average_distinct = 96;

  blr_function = 100;
  blr_gen_id = 101;
  blr_prot_mask = 102;
  blr_upcase = 103;
  blr_lock_state = 104;
  blr_value_if = 105;
  blr_matching2 = 106;
  blr_index = 107;
  blr_ansi_like = 108;
  blr_bookmark = 109;
  blr_crack = 110;
  blr_force_crack = 111;
  blr_seek = 112;
  blr_find = 113;

  blr_continue = 0;
  blr_forward = 1;
  blr_backward = 2;
  blr_bof_forward = 3;
  blr_eof_backward = 4;

  blr_lock_relation = 114;
  blr_lock_record = 115;
  blr_set_bookmark = 116;
  blr_get_bookmark = 117;
  blr_rs_stream = 119;
  blr_exec_proc = 120;
  blr_begin_range = 121;
  blr_end_range = 122;
  blr_delete_range = 123;
  blr_procedure = 124;
  blr_pid = 125;
  blr_exec_pid = 126;
  blr_singular = 127;
  blr_abort = 128;
  blr_block = 129;
  blr_error_handler = 130;
  blr_cast = 131;
  blr_release_lock = 132;
  blr_release_locks = 133;
  blr_start_savepoint = 134;
  blr_end_savepoint = 135;
  blr_find_dbkey = 136;
  blr_range_relation = 137;
  blr_delete_ranges = 138;

  blr_plan = 139;
  blr_merge = 140;
  blr_join = 141;
  blr_sequential = 142;
  blr_navigational = 143;
  blr_indices = 144;
  blr_retrieve = 145;

  blr_relation2 = 146;
  blr_rid2 = 147;
  blr_reset_stream = 148;
  blr_release_bookmark = 149;
  blr_set_generator = 150;
  blr_ansi_any = 151;
  blr_exists = 152;
  blr_cardinality = 153;

  blr_record_version = 154; // get tid of record
  blr_stall = 155; // fake server stall
  blr_seek_no_warn = 156;
  blr_find_dbkey_version = 157;
  blr_ansi_all = 158;
  blr_extract = 159;

{ sub parameters for blr_extract }
  blr_extract_year = 0;
  blr_extract_month = 1;
  blr_extract_day = 2;
  blr_extract_hour = 3;
  blr_extract_minute = 4;
  blr_extract_second = 5;
  blr_extract_weekday = 6;
  blr_extract_yearday = 7;

  blr_current_date = 160;
  blr_current_timestamp = 161;
  blr_current_time = 162;

 {******* These verbs were added in 6.0, primarily to support 64-bit integers *********}

  blr_add2 = 163;
  blr_subtract2 = 164;
  blr_multiply2 = 165;
  blr_divide2 = 166;
  blr_agg_total2 = 167;
  blr_agg_total_distinct2 = 168;
  blr_agg_average2 = 169;
  blr_agg_average_distinct2 = 170;
  blr_average2 = 171;
  blr_gen_id2 = 172;
  blr_set_generator2 = 173;
  
{****************** Plain API Types definition *****************}

type
{$IFDEF LINUX}
  ULong                = Cardinal;
  UChar                = Char;
  Short                = SmallInt;
{$ENDIF}
  ISC_LONG             = LongInt;
  UISC_LONG            = ULong;
  ISC_INT64            = TInt64;
  ISC_STATUS           = LongInt;
  UISC_STATUS          = ULong;
  PISC_LONG            = ^ISC_LONG;
  PUISC_LONG           = ^UISC_LONG;
  PISC_STATUS          = ^ISC_STATUS;
  PPISC_STATUS         = ^PISC_STATUS;
  PUISC_STATUS         = ^UISC_STATUS;
  PShort               = ^Short;
  PPChar               = ^PChar;
  UShort               = Word;
  PVoid                = Pointer;

  { C Date/Time Structure }
  TCTimeStructure = record
    tm_sec:        Integer;   { Seconds }
    tm_min:        Integer;   { Minutes }
    tm_hour:       Integer;   { Hour (0--23) }
    tm_mday:       Integer;   { Day of month (1--31) }
    tm_mon:        Integer;   { Month (0--11) }
    tm_year:       Integer;   { Year (calendar year minus 1900) }
    tm_wday:       Integer;   { Weekday (0--6) Sunday = 0) }
    tm_yday:       Integer;   { Day of year (0--365) }
    tm_isdst:      Integer;   { 0 if daylight savings time is not in effect) }
  end;
  PCTimeStructure = ^TCTimeStructure;
  TM = TCTimeStructure;
  PTM = ^TM;

  TISC_VARYING = record
    strlen:       Short;
    str:          array[0..0] of Char;
  end;

  { InterBase Handle Definitions }
  TISC_BLOB_HANDLE              = PVoid;
  PISC_BLOB_HANDLE              = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE                = PVoid;
  PISC_DB_HANDLE                = ^TISC_DB_HANDLE;
  TISC_STMT_HANDLE              = PVoid;
  PISC_STMT_HANDLE              = ^TISC_STMT_HANDLE;
  TISC_TR_HANDLE                = PVoid;
  PISC_TR_HANDLE                = ^TISC_TR_HANDLE;
  TISC_CALLBACK                 = procedure;

  { Time & Date Support }
  ISC_DATE = LongInt;
  PISC_DATE = ^ISC_DATE;
  ISC_TIME = ULong;
  PISC_TIME = ^ISC_TIME;

  TISC_TIMESTAMP = record
    timestamp_date:     ISC_DATE;
    timestamp_time:     ISC_TIME;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  { Blob id structure }
  TGDS_QUAD = record
    gds_quad_high:  ISC_LONG;
    gds_quad_low:   UISC_LONG;
  end;
  PGDS_QUAD            = ^TGDS_QUAD;

  TISC_QUAD            = TGDS_QUAD;
  PISC_QUAD            = ^TISC_QUAD;

  TISC_ARRAY_BOUND = record
    array_bound_lower:  Short;
    array_bound_upper:  Short;
  end;
  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;

  TISC_ARRAY_DESC = record
    array_desc_dtype:   UChar;
    array_desc_scale:   Char;
    array_desc_length:  Short;
    array_desc_field_name: array[0..31] of Char;
    array_desc_relation_name: array[0..31] of Char;
    array_desc_dimensions: Short;
    array_desc_flags: Short;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end;
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

  TISC_BLOB_DESC = record
    blob_desc_subtype:          Short;
    blob_desc_charset:          Short;
    blob_desc_segment_size:     Short;
    blob_desc_field_name:       array[0..31] of UChar;
    blob_desc_relation_name:    array[0..31] of UChar;
  end;
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

  { Declare the extended SQLDA }
  TXSQLVAR = record
    sqltype:            Short;     { datatype of field }
    sqlscale:           Short;     { scale factor }
    sqlsubtype:         Short;     { datatype subtype - BLOBs }
			           { & text types only }
    sqllen:             Short;     { length of data area }
    sqldata:            PChar;     { address of data }
    sqlind:             PSmallInt;  { address of indicator }
                                   { variable }
    sqlname_length:     Short;     { length of sqlname field }
    { name of field, name length + space for NULL }
    sqlname:            array[0..31] of Char;
    relname_length:     Short;     { length of relation name }
    { field's relation name + space for NULL }
    relname:            array[0..31] of Char;
    ownname_length:     Short;     { length of owner name }
    { relation's owner name + space for NULL }
    ownname:            array[0..31] of Char;
    aliasname_length:   Short;     { length of alias name }
    { relation's alias name + space for NULL }
    aliasname:          array[0..31] of Char;
  end;
  PXSQLVAR = ^TXSQLVAR;

  TXSQLDA = record
    version:            Short;     { version of this XSQLDA }
    { XSQLDA name field }
    sqldaid:            array[0..7] of Char;
    sqldabc:            ISC_LONG;  { length in bytes of SQLDA }
    sqln:               Short;     { number of fields allocated }
    sqld:               Short;     { actual number of fields }
    { first field address }
    sqlvar:             array[0..0] of TXSQLVAR;
  end;
  PXSQLDA = ^TXSQLDA;

 {****************************************************}
 { This record type is for passing arguments to       }
 { isc_start_transaction (See docs)                   }
 {****************************************************}
  TISC_START_TRANS = record
    db_handle:          PISC_DB_HANDLE;
    tpb_length:         Word;
    tpb_address:        PChar;
  end;

 {****************************************************}
 { This record type is for passing arguments to       }
 { isc_start_multiple (see docs)                      }
 {****************************************************}
  TISC_TEB = record
    db_handle:          PISC_DB_HANDLE;
    tpb_length:         LongInt;
    tpb_address:        PChar;
  end;
  PISC_TEB = ^TISC_TEB;
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

{************** Plain API Function types definition *************}

  { General database routines }

  Tisc_attach_database = function(status_vector: PISC_STATUS;
    db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
    parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_detach_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_drop_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_database_info = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PChar; result_buffer_length: Short;
    result_buffer: PChar): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  { Array processing routines }
  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS;
    isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
    isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_get_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_bounds = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PChar; 
    descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_desc = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PChar; 
    descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_set_desc = function(status_vector: PISC_STATUS;
    table_name: PChar; column_name: PChar;
    sql_dtype, sql_length, sql_dimensions: PShort;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_put_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_free = function(isc_arg1: PChar): ISC_LONG; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sql_interprete = procedure(sqlcode: Short; buffer: PChar;
    buffer_length: Short); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_interprete = function(buffer: PChar; status_vector: PPISC_STATUS):
    ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  { Transaction support routines }

  Tisc_start_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PChar):
    ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_start_multiple = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    teb_vector_address: PISC_TEB): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  { Dynamic SQL routines }

  Tisc_dsql_allocate_statement = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_alloc_statement2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe_bind = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
    xsqlda: PXSQLDA): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute2 = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
    in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
    statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_fetch = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_free_statement = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_prepare = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
    length: Word; statement: PChar; dialect: Word; xsqlda: PXSQLDA):
    ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_set_cursor_name = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: Word): ISC_STATUS;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_sql_info = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
    buffer_length: Short; buffer: PChar): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  { Blob processing routines }

  Tisc_open_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_buffer: PChar): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_create_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_address: PChar): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_blob_info = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar):
    ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_close_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_get_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
    seg_buffer_length: Word; seg_buffer: PChar): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_put_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PChar):
    ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  { Event processing routines }

  Tisc_event_block = function(event_buffer: PPChar; result_buffer: PPChar;
    id_count: Word; event_list: array of PChar): ISC_LONG; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_event_counts = procedure(status_vector: PISC_STATUS;
    buffer_length: Short; event_buffer: PChar; result_buffer: PChar); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_que_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
    event_buffer: PChar; event_function: TISC_CALLBACK;
    event_function_arg: PVoid): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  { Types convertion routines }

  Tisc_decode_date = procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  { Interbase Version 6 routines }
  Tisc_decode_sql_date = procedure(ib_date: PISC_DATE;
    tm_date: PCTimeStructure); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_sql_time = procedure(ib_time: PISC_TIME;
    tm_date: PCTimeStructure); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_timestamp = procedure(ib_timestamp: PISC_TIMESTAMP;
    tm_date: PCTimeStructure); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_date = procedure(tm_date: PCTimeStructure;
    ib_date: PISC_DATE); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_time = procedure(tm_date: PCTimeStructure;
    ib_time: PISC_TIME); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_timestamp = procedure(tm_date: PCTimeStructure;
    ib_timestamp: PISC_TIMESTAMP); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_vax_integer = function(buffer: PChar; length: Short): ISC_LONG; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

{************* Plain API Function variables definition ************}

var
  { General database routines }
  isc_attach_database:  Tisc_attach_database;
  isc_detach_database:  Tisc_detach_database;
  isc_drop_database:    Tisc_drop_database;
  isc_database_info:    Tisc_database_info;
  isc_free:             Tisc_free;
  isc_sqlcode:          Tisc_sqlcode;
  isc_sql_interprete:   Tisc_sql_interprete;
  isc_interprete:       Tisc_interprete;

  { Transaction support routines }
  isc_start_transaction: Tisc_start_transaction;
  isc_start_multiple:   Tisc_start_multiple;
  isc_rollback_transaction: Tisc_rollback_transaction;
  isc_rollback_retaining: Tisc_rollback_retaining;
  isc_commit_transaction: Tisc_commit_transaction;
  isc_commit_retaining: Tisc_commit_retaining;

  { Dynamic SQL routines }
  isc_dsql_allocate_statement: Tisc_dsql_allocate_statement;
  isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
  isc_dsql_describe:    Tisc_dsql_describe;
  isc_dsql_describe_bind: Tisc_dsql_describe_bind;
  isc_dsql_execute:     Tisc_dsql_execute;
  isc_dsql_execute2:    Tisc_dsql_execute2;
  isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
  isc_dsql_fetch:       Tisc_dsql_fetch;
  isc_dsql_free_statement: Tisc_dsql_free_statement;
  isc_dsql_prepare:     Tisc_dsql_prepare;
  isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
  isc_dsql_sql_info:    Tisc_dsql_sql_info;

  { Array processing routines }
  isc_array_gen_sdl:    Tisc_array_gen_sdl;
  isc_array_get_slice:  Tisc_array_get_slice;
  isc_array_lookup_bounds: Tisc_array_lookup_bounds;
  isc_array_lookup_desc: Tisc_array_lookup_desc;
  isc_array_set_desc:   Tisc_array_set_desc;
  isc_array_put_slice:  Tisc_array_put_slice;

  { Blob processing routines }
  isc_open_blob2:       Tisc_open_blob2;
  isc_create_blob2:     Tisc_create_blob2;
  isc_blob_info:        Tisc_blob_info;
  isc_close_blob:       Tisc_close_blob;
  isc_cancel_blob:      Tisc_cancel_blob;
  isc_get_segment:      Tisc_get_segment;
  isc_put_segment:      Tisc_put_segment;

  { Event processing routines }
  isc_que_events:       Tisc_que_events;
  isc_event_counts:     Tisc_event_counts;
  isc_event_block:      Tisc_event_block;
  isc_cancel_events:    Tisc_cancel_events;

  { Types convertion routines }
  isc_encode_date:      Tisc_encode_date;
  isc_decode_date:      Tisc_decode_date;
  isc_vax_integer:      Tisc_vax_integer;

  isc_encode_sql_date:  Tisc_encode_sql_date;
  isc_decode_sql_date:  Tisc_decode_sql_date;

  isc_encode_sql_time:  Tisc_encode_sql_time;
  isc_decode_sql_time:  Tisc_decode_sql_time;

  isc_encode_timestamp: Tisc_encode_timestamp;        
  isc_decode_timestamp: Tisc_decode_timestamp;

{ Library Initialization }
function IbSqlLoadLib: Boolean;

{ XSQLDA_LENGTH is defined in C as a macro, but in Pascal we must defined it
   as a function... }
function XSQLDA_LENGTH(Value: LongInt): LongInt;

function GetIbSqlClientVersion: Integer;
procedure CheckIbSqlLoaded;

const
  DLL: string = DEFAULT_DLL_LOCATION;
//  hDLL: THandle = 0;
//  ClientVersion: Integer = 0;

var
  hDLL: THandle;
  ClientVersion: Integer;
  LibLoaded: Boolean;

implementation

uses SysUtils, ZDBaseConst;

procedure CheckIbSqlLoaded;
begin
  if hDLL <= 0{HINSTANCE_ERROR} then
    IbSqlLoadLib;
  if hDLL <= 0{HINSTANCE_ERROR} then
    raise Exception.CreateFmt(SLibraryNotFound, [DLL]);
end;

function GetIbSqlClientVersion: Integer;
begin
  CheckIbSqlLoaded;
  result := ClientVersion;
end;

{***************** Stub Functions ***************}

function isc_rollback_retaining_stub(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_rollback_retaining']);
end;

procedure isc_encode_sql_date_stub(tm_date: PCTimeStructure;
  ib_date: PISC_DATE); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_encode_sql_date']);
end;

procedure isc_encode_sql_time_stub(tm_date: PCTimeStructure;
  ib_time: PISC_TIME); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_encode_sql_time']);
end;

procedure isc_encode_timestamp_stub(tm_date: PCTimeStructure;
  ib_timestamp: PISC_TIMESTAMP); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_encode_sql_timestamp']);
end;

procedure isc_decode_sql_date_stub(ib_date: PISC_DATE;
  tm_date: PCTimeStructure); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_decode_sql_date']);
end;

procedure isc_decode_sql_time_stub(ib_time: PISC_TIME;
  tm_date: PCTimeStructure); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_decode_sql_time']);
end;

procedure isc_decode_timestamp_stub(ib_timestamp: PISC_TIMESTAMP;
  tm_date: PCTimeStructure); {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};
begin
  raise Exception.CreateFmt('Feature %s is allowed only in Interbase 6.0',
    ['isc_decode_timestamp']);
end;

{ Initialize Interbase dynamic library }
function IbSqlLoadLib: Boolean;
begin
  if hDLL = 0 then
  begin
    hDLL := GetModuleHandle(PChar(DLL));
    LibLoaded := False;
    if hDLL = 0 then
    begin
      hDLL := LoadLibrary(PChar(DLL));
      LibLoaded := True;
    end;
  end;

  if hDLL <> 0 then
  begin
    isc_sqlcode         := GetProcAddress(hDLL,'isc_sqlcode');
    isc_sql_interprete  := GetProcAddress(hDLL,'isc_sql_interprete');
    isc_interprete      := GetProcAddress(hDLL,'isc_interprete');
    isc_vax_integer     := GetProcAddress(hDLL,'isc_vax_integer');

    isc_array_gen_sdl   := GetProcAddress(hDLL, 'isc_array_gen_sdl');
    isc_array_get_slice := GetProcAddress(hDLL, 'isc_array_get_slice');
    isc_array_lookup_bounds := GetProcAddress(hDLL, 'isc_array_lookup_bounds');
    isc_array_lookup_desc := GetProcAddress(hDLL, 'isc_array_lookup_desc');
    isc_array_set_desc  := GetProcAddress(hDLL, 'isc_array_set_desc');
    isc_array_put_slice := GetProcAddress(hDLL, 'isc_array_put_slice');

    isc_blob_info       := GetProcAddress(hDLL,'isc_blob_info');
    isc_open_blob2      := GetProcAddress(hDLL,'isc_open_blob2');
    isc_close_blob      := GetProcAddress(hDLL,'isc_close_blob');
    isc_cancel_blob     := GetProcAddress(hDLL,'isc_cancel_blob');
    isc_get_segment     := GetProcAddress(hDLL,'isc_get_segment');
    isc_put_segment     := GetProcAddress(hDLL,'isc_put_segment');
    isc_create_blob2    := GetProcAddress(hDLL,'isc_create_blob2');
    isc_decode_date     := GetProcAddress(hDLL,'isc_decode_date');
    isc_encode_date     := GetProcAddress(hDLL,'isc_encode_date');
    isc_dsql_free_statement := GetProcAddress(hDLL,'isc_dsql_free_statement');
    isc_dsql_execute2   := GetProcAddress(hDLL,'isc_dsql_execute2');
    isc_dsql_execute    := GetProcAddress(hDLL,'isc_dsql_execute');
    isc_dsql_set_cursor_name := GetProcAddress(hDLL,'isc_dsql_set_cursor_name');
    isc_dsql_fetch      := GetProcAddress(hDLL,'isc_dsql_fetch');
    isc_dsql_sql_info   := GetProcAddress(hDLL,'isc_dsql_sql_info');
    isc_dsql_allocate_statement := GetProcAddress(hDLL,'isc_dsql_allocate_statement');
    isc_dsql_alloc_statement2 := GetProcAddress(hDLL,'isc_dsql_alloc_statement2');
    isc_dsql_prepare    := GetProcAddress(hDLL,'isc_dsql_prepare');
    isc_dsql_describe_bind := GetProcAddress(hDLL,'isc_dsql_describe_bind');
    isc_dsql_describe   := GetProcAddress(hDLL,'isc_dsql_describe');
    isc_dsql_execute_immediate := GetProcAddress(hDLL,'isc_dsql_execute_immediate');
    isc_drop_database   := GetProcAddress(hDLL,'isc_drop_database');
    isc_detach_database := GetProcAddress(hDLL,'isc_detach_database');
    isc_attach_database := GetProcAddress(hDLL,'isc_attach_database');
    isc_database_info   := GetProcAddress(hDLL,'isc_database_info');
    isc_start_multiple  := GetProcAddress(hDLL,'isc_start_multiple');
    isc_start_transaction := GetProcAddress(hDLL,'isc_start_transaction');
    isc_commit_transaction := GetProcAddress(hDLL,'isc_commit_transaction');

    isc_commit_retaining := GetProcAddress(hDLL,'isc_commit_retaining');
    isc_rollback_transaction := GetProcAddress(hDLL,'isc_rollback_transaction');
    isc_cancel_events   := GetProcAddress(hDLL,'isc_cancel_events');
    isc_que_events      := GetProcAddress(hDLL,'isc_que_events');
    isc_event_counts    := GetProcAddress(hDLL,'isc_event_counts');
    isc_event_block     := GetProcAddress(hDLL,'isc_event_block');
    isc_free            := GetProcAddress(hDLL,'isc_free');

    isc_rollback_retaining := GetProcAddress(hDLL, 'isc_rollback_retaining');
    if Assigned(isc_rollback_retaining) then
    begin
      ClientVersion := 6;
      isc_decode_sql_date := GetProcAddress(hDLL, 'isc_decode_sql_date');
      isc_decode_sql_time := GetProcAddress(hDLL, 'isc_decode_sql_time');
      isc_decode_timestamp := GetProcAddress(hDLL, 'isc_decode_timestamp');
      isc_encode_sql_date := GetProcAddress(hDLL, 'isc_encode_sql_date');
      isc_encode_sql_time := GetProcAddress(hDLL, 'isc_encode_sql_time');
      isc_encode_timestamp := GetProcAddress(hDLL, 'isc_encode_timestamp');
    end
    else
    begin
      ClientVersion := 5;
      isc_rollback_retaining := isc_rollback_retaining_stub;

      isc_decode_sql_date := isc_decode_sql_date_stub;
      isc_decode_sql_time := isc_decode_sql_time_stub;
      isc_decode_timestamp := isc_decode_timestamp_stub;
      isc_encode_sql_date := isc_encode_sql_date_stub;
      isc_encode_sql_time := isc_encode_sql_time_stub;
      isc_encode_timestamp := isc_encode_timestamp_stub;
    end;

    Result := True;
  end
  else
    raise Exception.Create(Format(SLibraryNotFound, [DLL]));
end;

function XSQLDA_LENGTH(Value: LongInt): LongInt;
begin
  Result := SizeOf(TXSQLDA) + ((Value - 1) * SizeOf(TXSQLVAR));
end;

initialization
  hDLL := 0;
  ClientVersion := 0;
finalization
  if (hDLL <> 0) and LibLoaded then
    FreeLibrary(hDLL);
end.
