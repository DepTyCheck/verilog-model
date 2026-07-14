-- Seed: 11215444760261490720,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity oss is
  port ( tfuizyz : inout file_value_mirror
  ; nbdcx : inout record_value_mirror
  ; c : inout std_logic_vector(4 downto 3)
  ; oktcmyfb : buffer std_logic_vector(2 to 0)
  );
end oss;

architecture ellba of oss is
  
begin
  
end ellba;

entity ihd is
  port (kwl : linkage integer; hdi : linkage boolean);
end ihd;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture tdqlikjdlv of ihd is
  signal urmpb : std_logic_vector(2 to 0);
  signal qzn : std_logic_vector(4 downto 3);
  shared variable szrk : record_value_mirror;
  shared variable ral : file_value_mirror;
begin
  pibhzzvza : entity work.oss
    port map (tfuizyz => ral, nbdcx => szrk, c => qzn, oktcmyfb => urmpb);
  
  -- Multi-driven assignments
  urmpb <= (others => '0');
  qzn <= qzn;
  qzn <= ('X', '-');
  qzn <= qzn;
end tdqlikjdlv;

entity oabas is
  port (y : inout bit; nrebwuaor : out bit);
end oabas;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture rj of oabas is
  shared variable aystva : record_value_mirror;
  shared variable bsbumd : file_value_mirror;
  signal ikh : std_logic_vector(2 to 0);
  shared variable udufahx : record_value_mirror;
  shared variable czx : file_value_mirror;
  signal ev : std_logic_vector(2 to 0);
  signal dpks : std_logic_vector(4 downto 3);
  shared variable zp : record_value_mirror;
  shared variable sdzyfxby : file_value_mirror;
begin
  jfadjeqq : entity work.oss
    port map (tfuizyz => sdzyfxby, nbdcx => zp, c => dpks, oktcmyfb => ev);
  gbikmiy : entity work.oss
    port map (tfuizyz => czx, nbdcx => udufahx, c => dpks, oktcmyfb => ikh);
  tekugcy : entity work.oss
    port map (tfuizyz => bsbumd, nbdcx => aystva, c => dpks, oktcmyfb => ev);
  
  -- Single-driven assignments
  nrebwuaor <= nrebwuaor;
  y <= '1';
  
  -- Multi-driven assignments
  ev <= (others => '0');
  dpks <= "UH";
  dpks <= ('L', 'Z');
end rj;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity vqr is
  port (zfgtkj : in real; zwjedutkj : inout std_logic_vector(3 downto 3); sgmyjffj : linkage string(4 to 2); asr : inout value_mirror);
end vqr;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture zr of vqr is
  signal jvxrhhry : std_logic_vector(2 to 0);
  signal ciyaxxm : std_logic_vector(4 downto 3);
  shared variable t : record_value_mirror;
  shared variable co : file_value_mirror;
  signal b : boolean;
  signal lfeckwmnxn : integer;
  signal hjmigsi : std_logic_vector(2 to 0);
  signal txf : std_logic_vector(4 downto 3);
  shared variable eisz : record_value_mirror;
  shared variable qpzs : file_value_mirror;
begin
  ltpnjyauir : entity work.oss
    port map (tfuizyz => qpzs, nbdcx => eisz, c => txf, oktcmyfb => hjmigsi);
  fzu : entity work.ihd
    port map (kwl => lfeckwmnxn, hdi => b);
  hwtx : entity work.oss
    port map (tfuizyz => co, nbdcx => t, c => ciyaxxm, oktcmyfb => jvxrhhry);
  
  -- Multi-driven assignments
  zwjedutkj <= zwjedutkj;
end zr;



-- Seed after: 10144311065711244964,7726014785203345639
