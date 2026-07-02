-- Seed: 12577157500956774707,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity wny is
  port (jeryw : linkage integer; n : inout std_logic_vector(4 to 4));
end wny;

architecture o of wny is
  
begin
  -- Multi-driven assignments
  n <= "L";
  n <= (others => 'X');
  n <= (others => '0');
  n <= "0";
end o;

entity elywrxf is
  port (nj : linkage integer; islpg : linkage time; mjz : inout bit_vector(3 downto 0));
end elywrxf;

library ieee;
use ieee.std_logic_1164.all;

architecture ioupj of elywrxf is
  signal stnyydkq : integer;
  signal kyjajtv : std_logic_vector(4 to 4);
  signal szoupn : integer;
  signal ccngdmuwdv : integer;
  signal ae : std_logic_vector(4 to 4);
  signal bvvpvuukg : integer;
begin
  yj : entity work.wny
    port map (jeryw => bvvpvuukg, n => ae);
  v : entity work.wny
    port map (jeryw => ccngdmuwdv, n => ae);
  xvjoqe : entity work.wny
    port map (jeryw => szoupn, n => kyjajtv);
  nkjle : entity work.wny
    port map (jeryw => stnyydkq, n => ae);
  
  -- Single-driven assignments
  mjz <= ('1', '0', '0', '0');
  
  -- Multi-driven assignments
  ae <= "H";
  ae <= (others => 'U');
  ae <= (others => 'X');
end ioupj;

entity rjnigaqeui is
  port (rvy : in integer);
end rjnigaqeui;

library ieee;
use ieee.std_logic_1164.all;

architecture lsjyohygf of rjnigaqeui is
  signal nrxkxt : std_logic_vector(4 to 4);
  signal vxrdyyetw : integer;
  signal tcqhxe : std_logic_vector(4 to 4);
  signal yo : integer;
  signal vsyttnz : std_logic_vector(4 to 4);
  signal x : integer;
begin
  gf : entity work.wny
    port map (jeryw => x, n => vsyttnz);
  iwjjmtm : entity work.wny
    port map (jeryw => yo, n => tcqhxe);
  slgwi : entity work.wny
    port map (jeryw => vxrdyyetw, n => nrxkxt);
  
  -- Multi-driven assignments
  vsyttnz <= "0";
  vsyttnz <= (others => 'U');
  tcqhxe <= (others => 'H');
  vsyttnz <= (others => 'U');
end lsjyohygf;

library ieee;
use ieee.std_logic_1164.all;

entity nljyqdu is
  port (apm : buffer std_logic_vector(0 downto 1); imdjatu : buffer bit; bhppl : in real_vector(0 to 3));
end nljyqdu;

library ieee;
use ieee.std_logic_1164.all;

architecture vmeiwnl of nljyqdu is
  signal tkgqnat : integer;
  signal vjdkymrh : integer;
  signal yw : std_logic_vector(4 to 4);
  signal u : integer;
  signal frfr : bit_vector(3 downto 0);
  signal n : time;
  signal knyjm : integer;
begin
  fqssdv : entity work.elywrxf
    port map (nj => knyjm, islpg => n, mjz => frfr);
  zwrzapro : entity work.wny
    port map (jeryw => u, n => yw);
  v : entity work.rjnigaqeui
    port map (rvy => vjdkymrh);
  xd : entity work.rjnigaqeui
    port map (rvy => tkgqnat);
  
  -- Single-driven assignments
  vjdkymrh <= 8#3#;
  tkgqnat <= 2#11#;
  imdjatu <= '1';
  
  -- Multi-driven assignments
  yw <= (others => 'H');
end vmeiwnl;



-- Seed after: 9878480764872718636,13694093582652240945
