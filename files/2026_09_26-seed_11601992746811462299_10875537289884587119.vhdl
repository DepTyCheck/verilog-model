-- Seed: 11601992746811462299,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity aoift is
  port (xbi : out time; o : out real; uhrnhjpmn : buffer std_logic);
end aoift;

architecture nku of aoift is
  
begin
  -- Single-driven assignments
  o <= o;
  xbi <= xbi;
  
  -- Multi-driven assignments
  uhrnhjpmn <= uhrnhjpmn;
  uhrnhjpmn <= uhrnhjpmn;
end nku;

library ieee;
use ieee.std_logic_1164.all;

entity dahlqgkr is
  port (u : linkage std_logic_vector(1 downto 3); wsqxcqnqui : in time);
end dahlqgkr;

library ieee;
use ieee.std_logic_1164.all;

architecture mgekqo of dahlqgkr is
  signal nfee : std_logic;
  signal pwmsmj : real;
  signal p : time;
begin
  fpqpnnsmv : entity work.aoift
    port map (xbi => p, o => pwmsmj, uhrnhjpmn => nfee);
  
  -- Multi-driven assignments
  nfee <= nfee;
  nfee <= nfee;
end mgekqo;

library ieee;
use ieee.std_logic_1164.all;

entity hbtc is
  port (nf : linkage boolean; bdbvznz : buffer real; mcqmtxfzzo : out bit; rz : linkage std_logic_vector(3 downto 4));
end hbtc;

architecture chq of hbtc is
  signal fdc : time;
begin
  ozy : entity work.dahlqgkr
    port map (u => rz, wsqxcqnqui => fdc);
  
  -- Single-driven assignments
  bdbvznz <= bdbvznz;
  mcqmtxfzzo <= '0';
end chq;

library ieee;
use ieee.std_logic_1164.all;

entity rowttchb is
  port (l : out std_logic; zvybiczwc : linkage boolean);
end rowttchb;

library ieee;
use ieee.std_logic_1164.all;

architecture a of rowttchb is
  signal ewin : std_logic;
  signal ksg : real;
  signal hiz : real;
  signal ftjdnh : time;
  signal bhqdvdo : time;
  signal mnybyat : std_logic_vector(1 downto 3);
  signal jrkx : std_logic_vector(3 downto 4);
  signal t : bit;
  signal oy : real;
begin
  bi : entity work.hbtc
    port map (nf => zvybiczwc, bdbvznz => oy, mcqmtxfzzo => t, rz => jrkx);
  ktqbbvh : entity work.dahlqgkr
    port map (u => mnybyat, wsqxcqnqui => bhqdvdo);
  xgmadivzfy : entity work.aoift
    port map (xbi => ftjdnh, o => hiz, uhrnhjpmn => l);
  nsegn : entity work.aoift
    port map (xbi => bhqdvdo, o => ksg, uhrnhjpmn => ewin);
  
  -- Multi-driven assignments
  ewin <= l;
  mnybyat <= "";
  ewin <= l;
  l <= 'X';
end a;



-- Seed after: 17850874071544063866,10875537289884587119
