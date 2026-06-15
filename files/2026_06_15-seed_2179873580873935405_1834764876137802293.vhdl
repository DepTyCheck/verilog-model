-- Seed: 2179873580873935405,1834764876137802293

library ieee;
use ieee.std_logic_1164.all;

entity push is
  port ( mwizcdq : out std_logic
  ; mzttehgzkk : buffer boolean_vector(3 downto 4)
  ; ndai : out std_logic_vector(2 downto 4)
  ; tggmxwnzw : inout std_logic_vector(2 downto 1)
  );
end push;

architecture bp of push is
  
begin
  -- Single-driven assignments
  mzttehgzkk <= (others => TRUE);
  
  -- Multi-driven assignments
  ndai <= (others => '0');
  tggmxwnzw <= "WW";
  mwizcdq <= 'U';
end bp;

library ieee;
use ieee.std_logic_1164.all;

entity elgkjpxgv is
  port (ppapuyv : in real; ajiibjrg : inout std_logic_vector(2 to 1); nbswl : out std_logic; oqg : buffer std_logic);
end elgkjpxgv;

library ieee;
use ieee.std_logic_1164.all;

architecture pwu of elgkjpxgv is
  signal rsvliw : std_logic_vector(2 downto 1);
  signal lhw : boolean_vector(3 downto 4);
  signal suulzakkg : std_logic;
  signal wqmkiaqrvp : std_logic_vector(2 downto 4);
  signal cnxu : boolean_vector(3 downto 4);
  signal lqlha : std_logic;
  signal ioydfzrifc : std_logic_vector(2 downto 1);
  signal hlzhmjgyrc : std_logic_vector(2 downto 4);
  signal nmpmzohaqi : boolean_vector(3 downto 4);
begin
  oozfcn : entity work.push
    port map (mwizcdq => oqg, mzttehgzkk => nmpmzohaqi, ndai => hlzhmjgyrc, tggmxwnzw => ioydfzrifc);
  nvnj : entity work.push
    port map (mwizcdq => lqlha, mzttehgzkk => cnxu, ndai => wqmkiaqrvp, tggmxwnzw => ioydfzrifc);
  vrfqhzjiow : entity work.push
    port map (mwizcdq => suulzakkg, mzttehgzkk => lhw, ndai => wqmkiaqrvp, tggmxwnzw => rsvliw);
  
  -- Multi-driven assignments
  hlzhmjgyrc <= "";
  ajiibjrg <= (others => '0');
  oqg <= 'X';
  lqlha <= 'U';
end pwu;

entity tt is
  port (q : out real; aabu : buffer time; e : linkage real; tadzkpqzhw : inout real);
end tt;

library ieee;
use ieee.std_logic_1164.all;

architecture xvpeeikp of tt is
  signal bkpdulg : std_logic_vector(2 downto 1);
  signal lo : boolean_vector(3 downto 4);
  signal rm : std_logic;
  signal jdydmaz : std_logic_vector(2 downto 4);
  signal bb : boolean_vector(3 downto 4);
  signal dtmdfz : std_logic_vector(2 downto 1);
  signal jyctfup : std_logic_vector(2 downto 4);
  signal hc : boolean_vector(3 downto 4);
  signal mh : std_logic;
  signal zkzc : std_logic;
  signal c : std_logic_vector(2 to 1);
begin
  jim : entity work.elgkjpxgv
    port map (ppapuyv => tadzkpqzhw, ajiibjrg => c, nbswl => zkzc, oqg => mh);
  i : entity work.push
    port map (mwizcdq => zkzc, mzttehgzkk => hc, ndai => jyctfup, tggmxwnzw => dtmdfz);
  phnqfw : entity work.push
    port map (mwizcdq => zkzc, mzttehgzkk => bb, ndai => jdydmaz, tggmxwnzw => dtmdfz);
  lzoqczqz : entity work.push
    port map (mwizcdq => rm, mzttehgzkk => lo, ndai => jdydmaz, tggmxwnzw => bkpdulg);
  
  -- Single-driven assignments
  aabu <= 2#01100.000# fs;
  q <= 2#11010.11#;
  tadzkpqzhw <= 16#169.BC8EC#;
  
  -- Multi-driven assignments
  c <= "";
  c <= (others => '0');
  bkpdulg <= "L0";
  mh <= 'H';
end xvpeeikp;



-- Seed after: 1119862183301000422,1834764876137802293
