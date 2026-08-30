-- Seed: 12823928415011452057,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity lrmudo is
  port (noomtdaocn : linkage bit_vector(0 downto 2); vyq : buffer std_logic; ukxkuz : buffer integer);
end lrmudo;

architecture sf of lrmudo is
  
begin
  -- Single-driven assignments
  ukxkuz <= 16#3F#;
  
  -- Multi-driven assignments
  vyq <= 'U';
end sf;

library ieee;
use ieee.std_logic_1164.all;

entity netovtgyg is
  port (lvovr : inout std_logic_vector(0 downto 2));
end netovtgyg;

library ieee;
use ieee.std_logic_1164.all;

architecture exwltd of netovtgyg is
  signal mlftzmdlfb : integer;
  signal ydogcfc : std_logic;
  signal lflhlnll : bit_vector(0 downto 2);
begin
  wzsxmjh : entity work.lrmudo
    port map (noomtdaocn => lflhlnll, vyq => ydogcfc, ukxkuz => mlftzmdlfb);
  
  -- Multi-driven assignments
  lvovr <= (others => '0');
  ydogcfc <= ydogcfc;
  lvovr <= (others => '0');
end exwltd;

library ieee;
use ieee.std_logic_1164.all;

entity mloosan is
  port (khafcfiqv : inout time; yrcbnigses : inout integer; cjfgpqwwrx : out std_logic_vector(1 to 4); e : linkage severity_level);
end mloosan;

library ieee;
use ieee.std_logic_1164.all;

architecture wgxj of mloosan is
  signal vvw : integer;
  signal dgrls : std_logic;
  signal vwtnb : bit_vector(0 downto 2);
  signal jn : integer;
  signal njnsdqizw : bit_vector(0 downto 2);
  signal gpgk : std_logic_vector(0 downto 2);
  signal thzxuszfrb : integer;
  signal ippo : std_logic;
  signal dtmvz : bit_vector(0 downto 2);
begin
  zjaho : entity work.lrmudo
    port map (noomtdaocn => dtmvz, vyq => ippo, ukxkuz => thzxuszfrb);
  apcwe : entity work.netovtgyg
    port map (lvovr => gpgk);
  bnp : entity work.lrmudo
    port map (noomtdaocn => njnsdqizw, vyq => ippo, ukxkuz => jn);
  txnnbajqs : entity work.lrmudo
    port map (noomtdaocn => vwtnb, vyq => dgrls, ukxkuz => vvw);
  
  -- Single-driven assignments
  yrcbnigses <= jn;
  khafcfiqv <= khafcfiqv;
  
  -- Multi-driven assignments
  cjfgpqwwrx <= ('L', 'U', 'H', 'Z');
  ippo <= ippo;
  dgrls <= 'W';
  cjfgpqwwrx <= cjfgpqwwrx;
end wgxj;



-- Seed after: 13660975436935315515,4080032123900078489
