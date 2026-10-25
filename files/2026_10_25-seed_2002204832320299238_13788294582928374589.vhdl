-- Seed: 2002204832320299238,13788294582928374589

library ieee;
use ieee.std_logic_1164.all;

entity bl is
  port (bwkdyni : buffer std_logic; zbxpd : out time);
end bl;

architecture q of bl is
  
begin
  -- Single-driven assignments
  zbxpd <= 2#1_0_0_1.10# ps;
end q;

entity msmb is
  port (yzaet : buffer real; ro : in character; otlxcy : in time);
end msmb;

library ieee;
use ieee.std_logic_1164.all;

architecture eenu of msmb is
  signal nfplmvcwnq : time;
  signal gzgozxx : std_logic;
  signal a : time;
  signal gvyw : std_logic;
  signal fbfymzmb : time;
  signal vpvmms : std_logic;
  signal hlcpmqp : time;
  signal xjfczqlmzn : std_logic;
begin
  y : entity work.bl
    port map (bwkdyni => xjfczqlmzn, zbxpd => hlcpmqp);
  wovwtn : entity work.bl
    port map (bwkdyni => vpvmms, zbxpd => fbfymzmb);
  zfuc : entity work.bl
    port map (bwkdyni => gvyw, zbxpd => a);
  p : entity work.bl
    port map (bwkdyni => gzgozxx, zbxpd => nfplmvcwnq);
  
  -- Single-driven assignments
  yzaet <= yzaet;
  
  -- Multi-driven assignments
  xjfczqlmzn <= 'X';
end eenu;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (nhdzhqe : linkage std_logic; yij : out time; dnqyxwj : out std_logic; mo : inout character);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture msgk of k is
  signal wjxw : time;
  signal ymjwee : std_logic;
  signal hwpvlf : std_logic;
  signal atynesfp : time;
  signal jn : std_logic;
  signal uwmvfmk : time;
begin
  uycbkgaug : entity work.bl
    port map (bwkdyni => dnqyxwj, zbxpd => uwmvfmk);
  mfs : entity work.bl
    port map (bwkdyni => jn, zbxpd => atynesfp);
  ulnscxgtqp : entity work.bl
    port map (bwkdyni => hwpvlf, zbxpd => yij);
  wfvkyas : entity work.bl
    port map (bwkdyni => ymjwee, zbxpd => wjxw);
  
  -- Single-driven assignments
  mo <= 'w';
end msgk;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (agltf : inout std_logic_vector(0 to 3); bwljyntom : out integer; pyywgdx : buffer real; qg : in bit);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture whcf of o is
  signal j : time;
  signal ghqitubgb : std_logic;
  signal pnjtn : character;
  signal dkmo : time;
  signal eaqz : time;
  signal rh : std_logic;
begin
  doxibo : entity work.bl
    port map (bwkdyni => rh, zbxpd => eaqz);
  xbubzn : entity work.k
    port map (nhdzhqe => rh, yij => dkmo, dnqyxwj => rh, mo => pnjtn);
  qc : entity work.bl
    port map (bwkdyni => ghqitubgb, zbxpd => j);
  
  -- Single-driven assignments
  bwljyntom <= bwljyntom;
  pyywgdx <= 16#DA5.14B52#;
  
  -- Multi-driven assignments
  agltf <= agltf;
  rh <= 'H';
  ghqitubgb <= 'X';
end whcf;



-- Seed after: 8415082461055990802,13788294582928374589
