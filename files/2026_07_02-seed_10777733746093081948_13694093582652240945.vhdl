-- Seed: 10777733746093081948,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity nraigw is
  port (xwlno : out std_logic; kgu : inout integer; emgyelsod : linkage boolean_vector(3 downto 4); m : buffer std_logic_vector(4 to 3));
end nraigw;

architecture x of nraigw is
  
begin
  -- Multi-driven assignments
  m <= "";
  m <= "";
  m <= (others => '0');
end x;

entity ufviqkbcze is
  port (bkqea : out real; lbluvgbak : buffer time);
end ufviqkbcze;

library ieee;
use ieee.std_logic_1164.all;

architecture atnmuj of ufviqkbcze is
  signal rv : std_logic_vector(4 to 3);
  signal bqbncnzoxx : boolean_vector(3 downto 4);
  signal tp : integer;
  signal rurlnes : std_logic_vector(4 to 3);
  signal hgz : boolean_vector(3 downto 4);
  signal uyytmr : integer;
  signal wkof : std_logic;
  signal qeybd : std_logic_vector(4 to 3);
  signal wdjvgyl : boolean_vector(3 downto 4);
  signal oc : integer;
  signal ogboxram : std_logic_vector(4 to 3);
  signal hd : boolean_vector(3 downto 4);
  signal azo : integer;
  signal xiqcllbu : std_logic;
begin
  wrilnymbv : entity work.nraigw
    port map (xwlno => xiqcllbu, kgu => azo, emgyelsod => hd, m => ogboxram);
  ul : entity work.nraigw
    port map (xwlno => xiqcllbu, kgu => oc, emgyelsod => wdjvgyl, m => qeybd);
  aidjkap : entity work.nraigw
    port map (xwlno => wkof, kgu => uyytmr, emgyelsod => hgz, m => rurlnes);
  xsfjbbva : entity work.nraigw
    port map (xwlno => wkof, kgu => tp, emgyelsod => bqbncnzoxx, m => rv);
  
  -- Single-driven assignments
  bkqea <= 13014.3124;
  lbluvgbak <= 4_2_0.20244 ms;
  
  -- Multi-driven assignments
  ogboxram <= "";
  xiqcllbu <= '1';
  rv <= "";
end atnmuj;



-- Seed after: 10480986434437018502,13694093582652240945
