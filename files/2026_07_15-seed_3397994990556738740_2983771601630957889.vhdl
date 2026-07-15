-- Seed: 3397994990556738740,2983771601630957889

use std.reflection.all;

entity iasbd is
  port ( omjhxz : in bit_vector(4 downto 3)
  ; variable lrbjdcif : inout physical_value_mirror_pt
  ; variable qbdwtc : inout enumeration_subtype_mirror_pt
  ; variable uoyzil : inout integer_value_mirror_pt
  );
end iasbd;

architecture de of iasbd is
  
begin
  
end de;

use std.reflection.all;

entity ggadprcpe is
  port (kcgvq : out integer; nsxv : inout real; variable w : inout file_subtype_mirror_pt);
end ggadprcpe;

use std.reflection.all;

architecture mkfbbebfxc of ggadprcpe is
  shared variable exfnhugjt : integer_value_mirror_pt;
  shared variable uz : enumeration_subtype_mirror_pt;
  shared variable y : physical_value_mirror_pt;
  signal kytdods : bit_vector(4 downto 3);
  shared variable x : integer_value_mirror_pt;
  shared variable xzjjndjdw : enumeration_subtype_mirror_pt;
  shared variable jgansxabx : physical_value_mirror_pt;
  signal fzjvdloxr : bit_vector(4 downto 3);
begin
  tkt : entity work.iasbd
    port map (omjhxz => fzjvdloxr, lrbjdcif => jgansxabx, qbdwtc => xzjjndjdw, uoyzil => x);
  tbegnl : entity work.iasbd
    port map (omjhxz => kytdods, lrbjdcif => y, qbdwtc => uz, uoyzil => exfnhugjt);
  
  -- Single-driven assignments
  nsxv <= 16#A_D.371#;
  kytdods <= fzjvdloxr;
  fzjvdloxr <= ('1', '0');
  kcgvq <= kcgvq;
end mkfbbebfxc;

library ieee;
use ieee.std_logic_1164.all;

entity xiv is
  port (b : inout boolean_vector(2 downto 3); fy : inout time; adqvf : in std_logic);
end xiv;

use std.reflection.all;

architecture tmvmzagq of xiv is
  shared variable lok : integer_value_mirror_pt;
  shared variable btapjn : enumeration_subtype_mirror_pt;
  shared variable uey : physical_value_mirror_pt;
  shared variable txbfy : file_subtype_mirror_pt;
  signal xfr : real;
  signal kexgvxtavt : integer;
  shared variable ojm : file_subtype_mirror_pt;
  signal jhoirrmne : real;
  signal nypiombyh : integer;
  shared variable tsgncg : integer_value_mirror_pt;
  shared variable vjhkcgm : enumeration_subtype_mirror_pt;
  shared variable nx : physical_value_mirror_pt;
  signal shupt : bit_vector(4 downto 3);
begin
  drdohu : entity work.iasbd
    port map (omjhxz => shupt, lrbjdcif => nx, qbdwtc => vjhkcgm, uoyzil => tsgncg);
  ccjewupekp : entity work.ggadprcpe
    port map (kcgvq => nypiombyh, nsxv => jhoirrmne, w => ojm);
  przli : entity work.ggadprcpe
    port map (kcgvq => kexgvxtavt, nsxv => xfr, w => txbfy);
  esxda : entity work.iasbd
    port map (omjhxz => shupt, lrbjdcif => uey, qbdwtc => btapjn, uoyzil => lok);
end tmvmzagq;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity pfens is
  port (variable h : inout integer_value_mirror_pt; xwqgyd : inout real; vfx : out std_logic);
end pfens;

architecture coq of pfens is
  
begin
  -- Single-driven assignments
  xwqgyd <= 16#0_8.C_A_7#;
end coq;



-- Seed after: 11235630087222367690,2983771601630957889
