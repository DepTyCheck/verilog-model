-- Seed: 3532469671948954395,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity la is
  port (hpav : buffer std_logic_vector(3 to 2); ahobplptk : buffer std_logic; fvrcr : linkage real);
end la;

architecture jyyqzuq of la is
  
begin
  -- Multi-driven assignments
  ahobplptk <= 'L';
  ahobplptk <= '-';
  ahobplptk <= ahobplptk;
end jyyqzuq;

library ieee;
use ieee.std_logic_1164.all;

entity sepcttb is
  port (pxsuddovcf : out integer; dcb : inout std_logic; q : inout integer_vector(1 downto 0); ebvbhvjtt : buffer integer);
end sepcttb;

architecture azkrmvl of sepcttb is
  
begin
  -- Single-driven assignments
  q <= (8#7_2#, 331);
end azkrmvl;

library ieee;
use ieee.std_logic_1164.all;

entity hnqwivra is
  port (kuwkqtymf : inout std_logic_vector(0 downto 1));
end hnqwivra;

library ieee;
use ieee.std_logic_1164.all;

architecture lrhk of hnqwivra is
  signal m : real;
  signal s : std_logic_vector(3 to 2);
  signal nuvxc : real;
  signal qbzfr : real;
  signal sxzrtavnyz : std_logic;
  signal yuvla : std_logic_vector(3 to 2);
  signal hdlhadto : integer;
  signal nobewxxe : integer_vector(1 downto 0);
  signal xylskxi : std_logic;
  signal yrxdfgsnur : integer;
begin
  ki : entity work.sepcttb
    port map (pxsuddovcf => yrxdfgsnur, dcb => xylskxi, q => nobewxxe, ebvbhvjtt => hdlhadto);
  chb : entity work.la
    port map (hpav => yuvla, ahobplptk => sxzrtavnyz, fvrcr => qbzfr);
  gdyazxl : entity work.la
    port map (hpav => kuwkqtymf, ahobplptk => xylskxi, fvrcr => nuvxc);
  wdxlkdjn : entity work.la
    port map (hpav => s, ahobplptk => xylskxi, fvrcr => m);
end lrhk;

library ieee;
use ieee.std_logic_1164.all;

entity ckp is
  port (n : inout time; uhqsmlsf : inout std_logic_vector(0 to 4); jjqafsjb : buffer real; nyy : in time);
end ckp;

library ieee;
use ieee.std_logic_1164.all;

architecture oczuqindux of ckp is
  signal g : real;
  signal ljv : std_logic;
  signal meopnqt : std_logic_vector(3 to 2);
begin
  ihxkinda : entity work.la
    port map (hpav => meopnqt, ahobplptk => ljv, fvrcr => g);
  
  -- Single-driven assignments
  jjqafsjb <= jjqafsjb;
  n <= 0 sec;
  
  -- Multi-driven assignments
  uhqsmlsf <= "Z1UXZ";
  uhqsmlsf <= ('H', '-', 'U', 'H', '-');
  uhqsmlsf <= ('-', 'H', 'U', 'X', 'L');
  ljv <= ljv;
end oczuqindux;



-- Seed after: 1389457414882653137,5805648483995786113
