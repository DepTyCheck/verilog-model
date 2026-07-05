-- Seed: 6159458380515471871,3181554006726329157

use std.reflection.all;

entity bzldb is
  port (xclqqjmlcl : inout file_subtype_mirror);
end bzldb;

architecture xvjzp of bzldb is
  
begin
  
end xvjzp;

use std.reflection.all;

entity emi is
  port (pjcdmsyzs : inout integer_subtype_mirror; dbfckm : linkage time; ak : inout boolean);
end emi;

use std.reflection.all;

architecture hcgyge of emi is
  shared variable rjykxvuuw : file_subtype_mirror;
  shared variable dk : file_subtype_mirror;
  shared variable nosfdqk : file_subtype_mirror;
  shared variable azxenimj : file_subtype_mirror;
begin
  arvcfnws : entity work.bzldb
    port map (xclqqjmlcl => azxenimj);
  dtdblcj : entity work.bzldb
    port map (xclqqjmlcl => nosfdqk);
  qkysngvlw : entity work.bzldb
    port map (xclqqjmlcl => dk);
  kmmimrdvs : entity work.bzldb
    port map (xclqqjmlcl => rjykxvuuw);
  
  -- Single-driven assignments
  ak <= ak;
end hcgyge;

use std.reflection.all;

entity i is
  port (kzca : inout integer_subtype_mirror; czoftyam : inout bit_vector(2 to 1); tzks : inout protected_subtype_mirror; juruk : inout time);
end i;

use std.reflection.all;

architecture sucd of i is
  shared variable ntd : file_subtype_mirror;
  shared variable vfg : file_subtype_mirror;
begin
  aamizzw : entity work.bzldb
    port map (xclqqjmlcl => vfg);
  lilr : entity work.bzldb
    port map (xclqqjmlcl => ntd);
  
  -- Single-driven assignments
  juruk <= 8#5_4_0_7.277# fs;
  czoftyam <= (others => '0');
end sucd;

entity clefk is
  port (bpxim : out time);
end clefk;

use std.reflection.all;

architecture e of clefk is
  signal pulklidbey : boolean;
  shared variable evhpzdmy : integer_subtype_mirror;
  shared variable liabv : file_subtype_mirror;
  signal hsoytcqrbl : boolean;
  signal ii : time;
  shared variable no : integer_subtype_mirror;
begin
  eptnjnmk : entity work.emi
    port map (pjcdmsyzs => no, dbfckm => ii, ak => hsoytcqrbl);
  cnpala : entity work.bzldb
    port map (xclqqjmlcl => liabv);
  hjeb : entity work.emi
    port map (pjcdmsyzs => evhpzdmy, dbfckm => bpxim, ak => pulklidbey);
end e;



-- Seed after: 768184712154863666,3181554006726329157
