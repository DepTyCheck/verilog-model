-- Seed: 4742455267186461417,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity nve is
  port (zohwrd : in integer; ald : in time_vector(4 downto 1); zkujpg : linkage real; uvcjnlzv : in std_logic_vector(4 to 3));
end nve;

architecture jlt of nve is
  
begin
  
end jlt;

entity oxmmal is
  port (dadzpzxb : in time);
end oxmmal;

library ieee;
use ieee.std_logic_1164.all;

architecture dxwkmgp of oxmmal is
  signal soismsy : std_logic_vector(4 to 3);
  signal lwumkn : real;
  signal azwmamamg : integer;
  signal xgddfunhyv : real;
  signal e : time_vector(4 downto 1);
  signal yppztsgxdn : real;
  signal jubnw : integer;
  signal mgznoqqdo : std_logic_vector(4 to 3);
  signal nim : real;
  signal unbkl : time_vector(4 downto 1);
  signal hjtwu : integer;
begin
  ix : entity work.nve
    port map (zohwrd => hjtwu, ald => unbkl, zkujpg => nim, uvcjnlzv => mgznoqqdo);
  llwyf : entity work.nve
    port map (zohwrd => jubnw, ald => unbkl, zkujpg => yppztsgxdn, uvcjnlzv => mgznoqqdo);
  hlaqeb : entity work.nve
    port map (zohwrd => hjtwu, ald => e, zkujpg => xgddfunhyv, uvcjnlzv => mgznoqqdo);
  kta : entity work.nve
    port map (zohwrd => azwmamamg, ald => unbkl, zkujpg => lwumkn, uvcjnlzv => soismsy);
  
  -- Multi-driven assignments
  mgznoqqdo <= (others => '0');
end dxwkmgp;

library ieee;
use ieee.std_logic_1164.all;

entity yordbrgmig is
  port (bxikh : in bit; mde : buffer time; oldjuxqkuo : linkage std_logic);
end yordbrgmig;

library ieee;
use ieee.std_logic_1164.all;

architecture wkfcy of yordbrgmig is
  signal si : time;
  signal mxci : std_logic_vector(4 to 3);
  signal icxoo : real;
  signal ppl : time_vector(4 downto 1);
  signal oprgfdf : std_logic_vector(4 to 3);
  signal hsxbx : real;
  signal ipx : time_vector(4 downto 1);
  signal mtfiheu : integer;
begin
  zwuy : entity work.nve
    port map (zohwrd => mtfiheu, ald => ipx, zkujpg => hsxbx, uvcjnlzv => oprgfdf);
  cwaxhld : entity work.nve
    port map (zohwrd => mtfiheu, ald => ppl, zkujpg => icxoo, uvcjnlzv => mxci);
  dyvoifup : entity work.oxmmal
    port map (dadzpzxb => si);
  
  -- Single-driven assignments
  mtfiheu <= 21;
  ipx <= (8#1.6_3# ns, 2 min, 8#2# ns, 2#0_1_0.0_0_1# ns);
  mde <= 2.4 us;
  si <= 0 hr;
  
  -- Multi-driven assignments
  oprgfdf <= (others => '0');
  oprgfdf <= (others => '0');
end wkfcy;



-- Seed after: 17236993841441987941,14629254427735353553
