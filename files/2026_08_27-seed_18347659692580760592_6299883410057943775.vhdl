-- Seed: 18347659692580760592,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity qe is
  port (jlo : out std_logic_vector(3 downto 2); p : inout std_logic; irlaicdk : out time_vector(0 to 2));
end qe;

architecture nnxmmdnbvu of qe is
  
begin
  -- Single-driven assignments
  irlaicdk <= irlaicdk;
  
  -- Multi-driven assignments
  p <= p;
end nnxmmdnbvu;

entity vdvnjw is
  port (pvxsb : out character);
end vdvnjw;

library ieee;
use ieee.std_logic_1164.all;

architecture sf of vdvnjw is
  signal qibzhzij : time_vector(0 to 2);
  signal h : std_logic_vector(3 downto 2);
  signal qqa : time_vector(0 to 2);
  signal fwasq : std_logic;
  signal hbrwku : std_logic_vector(3 downto 2);
begin
  ngvga : entity work.qe
    port map (jlo => hbrwku, p => fwasq, irlaicdk => qqa);
  oiysymkia : entity work.qe
    port map (jlo => h, p => fwasq, irlaicdk => qibzhzij);
  
  -- Single-driven assignments
  pvxsb <= 'f';
end sf;



-- Seed after: 3275319680747139365,6299883410057943775
