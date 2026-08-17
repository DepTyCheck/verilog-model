-- Seed: 14553204881519869182,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity wwb is
  port (jxgz : inout real; yeqlohj : in std_logic_vector(1 downto 4); tjxbubz : in real);
end wwb;

architecture wmbuezgwpo of wwb is
  
begin
  -- Single-driven assignments
  jxgz <= 440.3420;
end wmbuezgwpo;

entity gltegj is
  port (xouuxgprcx : buffer string(2 downto 4));
end gltegj;

library ieee;
use ieee.std_logic_1164.all;

architecture tvhv of gltegj is
  signal gt : real;
  signal msuqfbspr : std_logic_vector(1 downto 4);
  signal by : real;
  signal evut : real;
  signal hcbewyh : real;
  signal owwwakq : std_logic_vector(1 downto 4);
  signal mnhpgofnbb : real;
  signal iwswqm : std_logic_vector(1 downto 4);
  signal euqpigd : real;
begin
  ftufed : entity work.wwb
    port map (jxgz => euqpigd, yeqlohj => iwswqm, tjxbubz => mnhpgofnbb);
  kbvbogyl : entity work.wwb
    port map (jxgz => mnhpgofnbb, yeqlohj => owwwakq, tjxbubz => mnhpgofnbb);
  gtdw : entity work.wwb
    port map (jxgz => hcbewyh, yeqlohj => iwswqm, tjxbubz => evut);
  elq : entity work.wwb
    port map (jxgz => by, yeqlohj => msuqfbspr, tjxbubz => gt);
  
  -- Single-driven assignments
  xouuxgprcx <= (others => ' ');
  gt <= 30.20001;
  evut <= 1100.0;
  
  -- Multi-driven assignments
  msuqfbspr <= iwswqm;
  iwswqm <= iwswqm;
end tvhv;

library ieee;
use ieee.std_logic_1164.all;

entity tb is
  port (yllbf : linkage std_logic_vector(3 downto 0); zkxwasmmf : inout time; maqmar : inout std_logic_vector(1 downto 1); lfdw : inout std_logic);
end tb;

library ieee;
use ieee.std_logic_1164.all;

architecture jywsejnnv of tb is
  signal mpnmriled : real;
  signal h : std_logic_vector(1 downto 4);
  signal gt : real;
  signal n : real;
  signal hwnq : std_logic_vector(1 downto 4);
  signal rhn : real;
  signal elldscavka : string(2 downto 4);
begin
  gg : entity work.gltegj
    port map (xouuxgprcx => elldscavka);
  wwckahy : entity work.wwb
    port map (jxgz => rhn, yeqlohj => hwnq, tjxbubz => n);
  gywkjed : entity work.wwb
    port map (jxgz => gt, yeqlohj => h, tjxbubz => n);
  jkqhpsdyy : entity work.wwb
    port map (jxgz => n, yeqlohj => hwnq, tjxbubz => mpnmriled);
  
  -- Single-driven assignments
  zkxwasmmf <= zkxwasmmf;
  mpnmriled <= rhn;
  
  -- Multi-driven assignments
  lfdw <= lfdw;
  lfdw <= 'H';
end jywsejnnv;



-- Seed after: 5031677549596449210,13843488114570579517
