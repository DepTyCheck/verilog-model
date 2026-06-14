-- Seed: 16616263436075221690,14652815260262078753

entity obzxk is
  port (hi : linkage integer; hez : buffer integer; ijpn : inout character);
end obzxk;

architecture dnb of obzxk is
  
begin
  -- Single-driven assignments
  ijpn <= 'p';
  hez <= 23000;
end dnb;

library ieee;
use ieee.std_logic_1164.all;

entity isgjnta is
  port (elmfid : buffer std_logic; qfhagmuot : linkage real; a : out time_vector(2 downto 4));
end isgjnta;

architecture vtkbou of isgjnta is
  signal lj : character;
  signal yf : integer;
  signal fbbvoizz : integer;
  signal gyg : character;
  signal kdrwcgpor : integer;
  signal zd : integer;
begin
  nxbsjfovh : entity work.obzxk
    port map (hi => zd, hez => kdrwcgpor, ijpn => gyg);
  plin : entity work.obzxk
    port map (hi => fbbvoizz, hez => yf, ijpn => lj);
  
  -- Single-driven assignments
  a <= (others => 0 ns);
  
  -- Multi-driven assignments
  elmfid <= '-';
  elmfid <= '1';
  elmfid <= 'H';
  elmfid <= 'L';
end vtkbou;

entity rv is
  port (cikyrgvx : buffer time);
end rv;

library ieee;
use ieee.std_logic_1164.all;

architecture vilbnpy of rv is
  signal xoeaeft : time_vector(2 downto 4);
  signal mf : real;
  signal zepuo : std_logic;
begin
  vlwfjtjbbi : entity work.isgjnta
    port map (elmfid => zepuo, qfhagmuot => mf, a => xoeaeft);
  
  -- Single-driven assignments
  cikyrgvx <= 8#41255# fs;
  
  -- Multi-driven assignments
  zepuo <= '1';
  zepuo <= '-';
  zepuo <= 'U';
  zepuo <= 'X';
end vilbnpy;

library ieee;
use ieee.std_logic_1164.all;

entity kfcy is
  port (szoijvioz : linkage std_logic_vector(3 downto 3));
end kfcy;

library ieee;
use ieee.std_logic_1164.all;

architecture ikthvyqtce of kfcy is
  signal rwk : character;
  signal unecxnr : integer;
  signal clcistq : integer;
  signal cwb : time;
  signal ssdwl : time;
  signal cfx : time_vector(2 downto 4);
  signal sbkygwey : real;
  signal cbmhlmg : std_logic;
begin
  jggtgxc : entity work.isgjnta
    port map (elmfid => cbmhlmg, qfhagmuot => sbkygwey, a => cfx);
  jj : entity work.rv
    port map (cikyrgvx => ssdwl);
  kozylnd : entity work.rv
    port map (cikyrgvx => cwb);
  kqn : entity work.obzxk
    port map (hi => clcistq, hez => unecxnr, ijpn => rwk);
  
  -- Multi-driven assignments
  cbmhlmg <= '1';
end ikthvyqtce;



-- Seed after: 13850455038985552511,14652815260262078753
