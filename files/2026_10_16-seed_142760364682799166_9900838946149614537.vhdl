-- Seed: 142760364682799166,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity nifdnm is
  port (idqzxdx : in integer; oip : linkage std_logic);
end nifdnm;

architecture ssaebjum of nifdnm is
  
begin
  
end ssaebjum;

library ieee;
use ieee.std_logic_1164.all;

entity xvek is
  port (dgtx : linkage time_vector(2 downto 1); iclrpu : in std_logic_vector(0 to 2); dwos : inout time_vector(2 to 2));
end xvek;

library ieee;
use ieee.std_logic_1164.all;

architecture tv of xvek is
  signal devvsm : integer;
  signal xsxv : integer;
  signal xblk : integer;
  signal qnjeymy : std_logic;
  signal pmbmg : integer;
begin
  sjpwimxp : entity work.nifdnm
    port map (idqzxdx => pmbmg, oip => qnjeymy);
  fattiszmus : entity work.nifdnm
    port map (idqzxdx => xblk, oip => qnjeymy);
  l : entity work.nifdnm
    port map (idqzxdx => xsxv, oip => qnjeymy);
  rkzy : entity work.nifdnm
    port map (idqzxdx => devvsm, oip => qnjeymy);
  
  -- Single-driven assignments
  xblk <= 1;
  
  -- Multi-driven assignments
  qnjeymy <= qnjeymy;
  qnjeymy <= '-';
  qnjeymy <= qnjeymy;
  qnjeymy <= 'U';
end tv;

entity hjhke is
  port (ilpqbtbdtz : in real_vector(1 downto 4); lya : linkage integer_vector(4 to 1); gjgj : inout real);
end hjhke;

library ieee;
use ieee.std_logic_1164.all;

architecture fsm of hjhke is
  signal dtxtrzvx : std_logic;
  signal btunsvwa : integer;
  signal amjkg : time_vector(2 to 2);
  signal yysilobdy : time_vector(2 downto 1);
  signal teknpau : time_vector(2 to 2);
  signal moycb : std_logic_vector(0 to 2);
  signal gobo : time_vector(2 downto 1);
  signal kcxbk : time_vector(2 to 2);
  signal mb : std_logic_vector(0 to 2);
  signal uk : time_vector(2 downto 1);
begin
  rmxxqos : entity work.xvek
    port map (dgtx => uk, iclrpu => mb, dwos => kcxbk);
  amxk : entity work.xvek
    port map (dgtx => gobo, iclrpu => moycb, dwos => teknpau);
  eeczijat : entity work.xvek
    port map (dgtx => yysilobdy, iclrpu => mb, dwos => amjkg);
  xadiws : entity work.nifdnm
    port map (idqzxdx => btunsvwa, oip => dtxtrzvx);
  
  -- Multi-driven assignments
  dtxtrzvx <= 'Z';
  mb <= mb;
  moycb <= mb;
end fsm;



-- Seed after: 11855851273292780431,9900838946149614537
