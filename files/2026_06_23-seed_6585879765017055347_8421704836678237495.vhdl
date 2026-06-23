-- Seed: 6585879765017055347,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity qpdt is
  port (zoaylwwp : inout time; lnyhrtrsms : buffer bit; brrk : linkage severity_level; x : buffer std_logic);
end qpdt;

architecture gfig of qpdt is
  
begin
  -- Single-driven assignments
  lnyhrtrsms <= '0';
  zoaylwwp <= 20 ps;
  
  -- Multi-driven assignments
  x <= 'Z';
  x <= 'L';
end gfig;

library ieee;
use ieee.std_logic_1164.all;

entity oktdyzdrd is
  port (kjqrec : in real_vector(3 to 0); gza : out std_logic);
end oktdyzdrd;

architecture djmje of oktdyzdrd is
  signal yed : severity_level;
  signal bs : bit;
  signal xx : time;
  signal ymmwy : severity_level;
  signal uyffdduur : bit;
  signal ayhntlvqi : time;
  signal rnmq : severity_level;
  signal wgpkvkka : bit;
  signal i : time;
begin
  fzhsb : entity work.qpdt
    port map (zoaylwwp => i, lnyhrtrsms => wgpkvkka, brrk => rnmq, x => gza);
  svqlpzwjx : entity work.qpdt
    port map (zoaylwwp => ayhntlvqi, lnyhrtrsms => uyffdduur, brrk => ymmwy, x => gza);
  decgpah : entity work.qpdt
    port map (zoaylwwp => xx, lnyhrtrsms => bs, brrk => yed, x => gza);
  
  -- Multi-driven assignments
  gza <= '-';
  gza <= 'Z';
end djmje;



-- Seed after: 9176421000466892481,8421704836678237495
