-- Seed: 10078652321009205682,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity ryg is
  port (zus : out boolean; dgsuxko : inout std_logic; hsptmn : in time);
end ryg;

architecture dhiafo of ryg is
  
begin
  -- Single-driven assignments
  zus <= TRUE;
  
  -- Multi-driven assignments
  dgsuxko <= '0';
  dgsuxko <= 'H';
  dgsuxko <= 'W';
  dgsuxko <= 'L';
end dhiafo;

library ieee;
use ieee.std_logic_1164.all;

entity hign is
  port (uyxd : inout std_logic; shfphge : inout boolean; ieikfthlz : inout boolean; gsasgayyjc : buffer boolean_vector(2 to 1));
end hign;

library ieee;
use ieee.std_logic_1164.all;

architecture y of hign is
  signal jgsjpdaly : time;
  signal fmlllvisdz : std_logic;
  signal aezxmacg : boolean;
  signal mxkxskakf : time;
  signal b : time;
  signal cjk : std_logic;
  signal rs : boolean;
  signal gbwq : time;
  signal vwkvshonf : std_logic;
  signal hzgdelsjx : boolean;
begin
  rva : entity work.ryg
    port map (zus => hzgdelsjx, dgsuxko => vwkvshonf, hsptmn => gbwq);
  zirk : entity work.ryg
    port map (zus => rs, dgsuxko => cjk, hsptmn => b);
  lggvaaxs : entity work.ryg
    port map (zus => ieikfthlz, dgsuxko => vwkvshonf, hsptmn => mxkxskakf);
  wzxkloe : entity work.ryg
    port map (zus => aezxmacg, dgsuxko => fmlllvisdz, hsptmn => jgsjpdaly);
  
  -- Single-driven assignments
  shfphge <= FALSE;
  b <= 0 hr;
  
  -- Multi-driven assignments
  uyxd <= 'U';
  uyxd <= '1';
end y;

entity gacdicoov is
  port (dezexyvvz : out integer);
end gacdicoov;

library ieee;
use ieee.std_logic_1164.all;

architecture hkjf of gacdicoov is
  signal whwfeyj : boolean_vector(2 to 1);
  signal d : boolean;
  signal zyqhj : boolean;
  signal vha : std_logic;
begin
  ldogfs : entity work.hign
    port map (uyxd => vha, shfphge => zyqhj, ieikfthlz => d, gsasgayyjc => whwfeyj);
  
  -- Multi-driven assignments
  vha <= '-';
  vha <= 'L';
end hkjf;



-- Seed after: 17497172435460669808,17924494779688682807
