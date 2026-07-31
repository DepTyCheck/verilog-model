-- Seed: 15191738868563203886,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity jxsjgwl is
  port (ssqesaws : inout std_logic_vector(4 downto 0));
end jxsjgwl;

architecture uukbfv of jxsjgwl is
  
begin
  -- Multi-driven assignments
  ssqesaws <= "ZHL-H";
  ssqesaws <= ('Z', '0', '-', '0', 'X');
end uukbfv;

library ieee;
use ieee.std_logic_1164.all;

entity qntssmeynh is
  port ( yjlg : out boolean_vector(2 to 0)
  ; dr : in bit_vector(3 downto 2)
  ; meiumvfqt : in std_logic_vector(2 to 2)
  ; c : in std_logic_vector(2 downto 0)
  );
end qntssmeynh;

library ieee;
use ieee.std_logic_1164.all;

architecture i of qntssmeynh is
  signal otajp : std_logic_vector(4 downto 0);
begin
  vrbgcso : entity work.jxsjgwl
    port map (ssqesaws => otajp);
  
  -- Single-driven assignments
  yjlg <= yjlg;
  
  -- Multi-driven assignments
  otajp <= ('-', 'Z', '1', 'H', '1');
end i;

entity ntjptfads is
  port (a : buffer integer);
end ntjptfads;

library ieee;
use ieee.std_logic_1164.all;

architecture ndkgwjap of ntjptfads is
  signal uqeehzr : std_logic_vector(4 downto 0);
begin
  bc : entity work.jxsjgwl
    port map (ssqesaws => uqeehzr);
  
  -- Single-driven assignments
  a <= 16#20#;
  
  -- Multi-driven assignments
  uqeehzr <= uqeehzr;
  uqeehzr <= "W1W1H";
  uqeehzr <= uqeehzr;
end ndkgwjap;

entity dxbowiea is
  port (mjgxl : in real);
end dxbowiea;

library ieee;
use ieee.std_logic_1164.all;

architecture ncbjb of dxbowiea is
  signal qivclcri : std_logic_vector(2 downto 0);
  signal yiyncci : std_logic_vector(2 to 2);
  signal oiqufvaatv : bit_vector(3 downto 2);
  signal vknir : boolean_vector(2 to 0);
  signal pxnkxevj : std_logic_vector(4 downto 0);
  signal uhjvrl : std_logic_vector(4 downto 0);
begin
  qb : entity work.jxsjgwl
    port map (ssqesaws => uhjvrl);
  wlafbxipt : entity work.jxsjgwl
    port map (ssqesaws => pxnkxevj);
  k : entity work.qntssmeynh
    port map (yjlg => vknir, dr => oiqufvaatv, meiumvfqt => yiyncci, c => qivclcri);
  
  -- Single-driven assignments
  oiqufvaatv <= ('1', '0');
end ncbjb;



-- Seed after: 8111894181585664632,4177195558088809003
