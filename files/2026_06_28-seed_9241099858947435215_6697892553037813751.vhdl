-- Seed: 9241099858947435215,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity ksortq is
  port (ntgxpapj : inout std_logic_vector(2 downto 4); zafgq : inout time; nsenqwn : buffer std_logic; nxjbzbqo : out time);
end ksortq;

architecture wdzwk of ksortq is
  
begin
  -- Single-driven assignments
  zafgq <= 0_1 ns;
  nxjbzbqo <= 16#9_F_4.0_E_E# us;
end wdzwk;

library ieee;
use ieee.std_logic_1164.all;

entity ucatox is
  port (djturboq : out time; ppstmpstcg : in integer_vector(4 to 2); zsax : linkage std_logic);
end ucatox;

library ieee;
use ieee.std_logic_1164.all;

architecture oogpprwa of ucatox is
  signal xzslpwxvc : std_logic;
  signal gsrwuvs : time;
  signal orcydbv : std_logic_vector(2 downto 4);
begin
  jkg : entity work.ksortq
    port map (ntgxpapj => orcydbv, zafgq => gsrwuvs, nsenqwn => xzslpwxvc, nxjbzbqo => djturboq);
  
  -- Multi-driven assignments
  orcydbv <= "";
end oogpprwa;

library ieee;
use ieee.std_logic_1164.all;

entity cb is
  port (yaqqm : linkage std_logic; fxkqmugcz : in integer; emqa : linkage integer);
end cb;

library ieee;
use ieee.std_logic_1164.all;

architecture lzemahmnu of cb is
  signal nlhvpbwef : std_logic;
  signal rjxaq : integer_vector(4 to 2);
  signal uzoqdxjc : time;
  signal guca : time;
  signal joa : time;
  signal fpwamrbg : std_logic_vector(2 downto 4);
  signal uero : time;
  signal fjag : std_logic;
  signal jwkrfekyb : time;
  signal rsfi : std_logic_vector(2 downto 4);
  signal wpqeklf : std_logic;
  signal pgiry : integer_vector(4 to 2);
  signal hopn : time;
begin
  y : entity work.ucatox
    port map (djturboq => hopn, ppstmpstcg => pgiry, zsax => wpqeklf);
  aswrbcayqw : entity work.ksortq
    port map (ntgxpapj => rsfi, zafgq => jwkrfekyb, nsenqwn => fjag, nxjbzbqo => uero);
  zxjcrev : entity work.ksortq
    port map (ntgxpapj => fpwamrbg, zafgq => joa, nsenqwn => fjag, nxjbzbqo => guca);
  qflq : entity work.ucatox
    port map (djturboq => uzoqdxjc, ppstmpstcg => rjxaq, zsax => nlhvpbwef);
  
  -- Single-driven assignments
  rjxaq <= (others => 0);
  pgiry <= (others => 0);
  
  -- Multi-driven assignments
  nlhvpbwef <= 'W';
  fjag <= 'Z';
end lzemahmnu;

entity xtknbvrk is
  port (szwpyghg : linkage time; lvhqawffhi : buffer string(4 downto 5));
end xtknbvrk;

library ieee;
use ieee.std_logic_1164.all;

architecture hoqsgmmqoq of xtknbvrk is
  signal i : integer;
  signal hieug : time;
  signal fyfdixn : std_logic;
  signal osm : time;
  signal kqkxsmxfz : std_logic_vector(2 downto 4);
begin
  ikzdy : entity work.ksortq
    port map (ntgxpapj => kqkxsmxfz, zafgq => osm, nsenqwn => fyfdixn, nxjbzbqo => hieug);
  mxvoprg : entity work.cb
    port map (yaqqm => fyfdixn, fxkqmugcz => i, emqa => i);
  
  -- Single-driven assignments
  lvhqawffhi <= "";
  
  -- Multi-driven assignments
  fyfdixn <= '0';
  kqkxsmxfz <= (others => '0');
  kqkxsmxfz <= (others => '0');
  fyfdixn <= '0';
end hoqsgmmqoq;



-- Seed after: 3092658255561420200,6697892553037813751
