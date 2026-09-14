-- Seed: 8339997560951641094,13196211255131729027

entity mdnd is
  port (l : inout real);
end mdnd;

architecture hlnvsy of mdnd is
  
begin
  -- Single-driven assignments
  l <= l;
end hlnvsy;

library ieee;
use ieee.std_logic_1164.all;

entity om is
  port (qaobr : out integer; bcq : linkage std_logic);
end om;

architecture zwerj of om is
  
begin
  -- Single-driven assignments
  qaobr <= 0_1_2_3_0;
end zwerj;

library ieee;
use ieee.std_logic_1164.all;

entity qjqm is
  port (gvkhu : in std_logic; ivcnsbjq : inout time; jivbsclnf : linkage bit_vector(2 to 1); jqc : inout time);
end qjqm;

architecture gv of qjqm is
  signal fdoq : real;
begin
  lu : entity work.mdnd
    port map (l => fdoq);
  
  -- Single-driven assignments
  ivcnsbjq <= jqc;
end gv;

library ieee;
use ieee.std_logic_1164.all;

entity ujev is
  port (v : linkage std_logic_vector(0 downto 0); df : out integer; gitmeqvek : in std_logic_vector(4 to 2); gqmtvkcg : linkage real);
end ujev;

library ieee;
use ieee.std_logic_1164.all;

architecture laf of ujev is
  signal vxcgxh : time;
  signal wcnve : bit_vector(2 to 1);
  signal leb : time;
  signal peoutndu : std_logic;
  signal tryufhcfln : real;
  signal epcuaw : real;
  signal uovjoemkzc : real;
begin
  fxgvmipje : entity work.mdnd
    port map (l => uovjoemkzc);
  hiznrblbxa : entity work.mdnd
    port map (l => epcuaw);
  tdtsczop : entity work.mdnd
    port map (l => tryufhcfln);
  vfsiwpga : entity work.qjqm
    port map (gvkhu => peoutndu, ivcnsbjq => leb, jivbsclnf => wcnve, jqc => vxcgxh);
  
  -- Single-driven assignments
  df <= 16#1_2#;
  
  -- Multi-driven assignments
  peoutndu <= 'L';
  peoutndu <= peoutndu;
end laf;



-- Seed after: 14538672751214372072,13196211255131729027
