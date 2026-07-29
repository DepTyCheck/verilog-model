-- Seed: 471493557275278784,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity cdyxqj is
  port (dlcfgaozea : out string(1 downto 2); kxlkz : in integer; oesjtkga : in std_logic);
end cdyxqj;

architecture zcyqmg of cdyxqj is
  
begin
  -- Single-driven assignments
  dlcfgaozea <= "";
end zcyqmg;

entity bskso is
  port (jilalqba : in time);
end bskso;

library ieee;
use ieee.std_logic_1164.all;

architecture rs of bskso is
  signal pt : std_logic;
  signal nhau : integer;
  signal gcxrljdwmj : string(1 downto 2);
begin
  buicd : entity work.cdyxqj
    port map (dlcfgaozea => gcxrljdwmj, kxlkz => nhau, oesjtkga => pt);
  
  -- Single-driven assignments
  nhau <= nhau;
  
  -- Multi-driven assignments
  pt <= 'L';
  pt <= 'W';
  pt <= 'L';
end rs;

library ieee;
use ieee.std_logic_1164.all;

entity yw is
  port (zcl : out std_logic_vector(4 to 1); fc : out time; dwsfcxkwdy : in std_logic; l : out integer_vector(4 to 3));
end yw;

architecture ssoq of yw is
  signal qf : integer;
  signal kujfdyrntz : string(1 downto 2);
  signal w : integer;
  signal f : string(1 downto 2);
  signal d : string(1 downto 2);
  signal vjolnakue : integer;
  signal hkhsxzamo : string(1 downto 2);
begin
  wka : entity work.cdyxqj
    port map (dlcfgaozea => hkhsxzamo, kxlkz => vjolnakue, oesjtkga => dwsfcxkwdy);
  jw : entity work.cdyxqj
    port map (dlcfgaozea => d, kxlkz => vjolnakue, oesjtkga => dwsfcxkwdy);
  hpyhp : entity work.cdyxqj
    port map (dlcfgaozea => f, kxlkz => w, oesjtkga => dwsfcxkwdy);
  nrdjy : entity work.cdyxqj
    port map (dlcfgaozea => kujfdyrntz, kxlkz => qf, oesjtkga => dwsfcxkwdy);
  
  -- Single-driven assignments
  l <= (others => 0);
  w <= 8#3#;
  vjolnakue <= vjolnakue;
  fc <= 2#1101# ns;
  
  -- Multi-driven assignments
  zcl <= (others => '0');
end ssoq;

library ieee;
use ieee.std_logic_1164.all;

entity njfwkaqglb is
  port (istiwwh : out std_logic);
end njfwkaqglb;

library ieee;
use ieee.std_logic_1164.all;

architecture uhx of njfwkaqglb is
  signal fwfjqcr : integer_vector(4 to 3);
  signal eng : time;
  signal gzdyrri : std_logic_vector(4 to 1);
begin
  xrghcqwd : entity work.yw
    port map (zcl => gzdyrri, fc => eng, dwsfcxkwdy => istiwwh, l => fwfjqcr);
  kw : entity work.bskso
    port map (jilalqba => eng);
  
  -- Multi-driven assignments
  istiwwh <= 'U';
end uhx;



-- Seed after: 11564949981157381702,14641901754878719179
