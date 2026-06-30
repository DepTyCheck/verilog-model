-- Seed: 14046091510919998395,14629254427735353553

entity vbkrifrzlp is
  port (xlh : inout time);
end vbkrifrzlp;

architecture qa of vbkrifrzlp is
  
begin
  
end qa;

entity j is
  port (a : inout character);
end j;

architecture evqiyn of j is
  
begin
  -- Single-driven assignments
  a <= 'c';
end evqiyn;

entity hrqleqa is
  port (ekwahixlnf : linkage integer; wx : buffer integer; grp : out integer; harblo : out time);
end hrqleqa;

architecture meavak of hrqleqa is
  signal w : character;
  signal wnco : time;
begin
  b : entity work.vbkrifrzlp
    port map (xlh => wnco);
  esho : entity work.vbkrifrzlp
    port map (xlh => harblo);
  yrydqu : entity work.j
    port map (a => w);
  
  -- Single-driven assignments
  grp <= 3;
  wx <= 2#0_1_0#;
end meavak;

library ieee;
use ieee.std_logic_1164.all;

entity mhxt is
  port (yssxjffwg : out string(3 downto 1); xqvmseyui : linkage std_logic; x : buffer std_logic_vector(2 to 3));
end mhxt;

architecture djkpgip of mhxt is
  signal jqjbpfz : character;
  signal fiqjcihuc : time;
  signal sfijjlo : time;
  signal hprwcaxhu : integer;
  signal hpnaaokju : integer;
  signal qdbaahaqgy : integer;
begin
  cqvcbdm : entity work.hrqleqa
    port map (ekwahixlnf => qdbaahaqgy, wx => hpnaaokju, grp => hprwcaxhu, harblo => sfijjlo);
  h : entity work.vbkrifrzlp
    port map (xlh => fiqjcihuc);
  itswingcii : entity work.j
    port map (a => jqjbpfz);
  
  -- Single-driven assignments
  yssxjffwg <= ('m', 'r', 'w');
  
  -- Multi-driven assignments
  x <= "LL";
  x <= "U1";
end djkpgip;



-- Seed after: 16572744567521514610,14629254427735353553
