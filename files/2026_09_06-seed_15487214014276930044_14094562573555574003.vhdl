-- Seed: 15487214014276930044,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity saqmn is
  port (lncyyu : in boolean_vector(0 to 4); h : buffer std_logic_vector(4 downto 4); uejcnlcc : buffer std_logic; bocbeyq : inout character);
end saqmn;

architecture usjxo of saqmn is
  
begin
  -- Single-driven assignments
  bocbeyq <= 'e';
  
  -- Multi-driven assignments
  uejcnlcc <= uejcnlcc;
  uejcnlcc <= 'X';
  h <= h;
  h <= "0";
end usjxo;

library ieee;
use ieee.std_logic_1164.all;

entity gxbkteu is
  port (aluxh : out std_logic; jzbsha : in real);
end gxbkteu;

library ieee;
use ieee.std_logic_1164.all;

architecture sayacrcuia of gxbkteu is
  signal yioilyr : character;
  signal bwd : std_logic;
  signal bnit : std_logic_vector(4 downto 4);
  signal idpjnrfd : boolean_vector(0 to 4);
  signal iounksms : character;
  signal xkbislhni : std_logic_vector(4 downto 4);
  signal ocvyukt : boolean_vector(0 to 4);
begin
  lwxnbnbda : entity work.saqmn
    port map (lncyyu => ocvyukt, h => xkbislhni, uejcnlcc => aluxh, bocbeyq => iounksms);
  eeme : entity work.saqmn
    port map (lncyyu => idpjnrfd, h => bnit, uejcnlcc => bwd, bocbeyq => yioilyr);
  
  -- Single-driven assignments
  ocvyukt <= (FALSE, FALSE, TRUE, FALSE, TRUE);
  idpjnrfd <= (TRUE, FALSE, TRUE, TRUE, TRUE);
end sayacrcuia;

entity jdlzshuw is
  port (pq : out integer_vector(2 to 4); imqijeoaa : out character; sspj : linkage real_vector(4 to 3));
end jdlzshuw;

library ieee;
use ieee.std_logic_1164.all;

architecture moyw of jdlzshuw is
  signal yzqyc : boolean_vector(0 to 4);
  signal fqwvp : character;
  signal mdhwl : character;
  signal mm : std_logic;
  signal zrz : std_logic_vector(4 downto 4);
  signal lmveeww : boolean_vector(0 to 4);
  signal bid : character;
  signal zvstx : std_logic;
  signal wiz : std_logic_vector(4 downto 4);
  signal mxumexs : boolean_vector(0 to 4);
begin
  zliabjweth : entity work.saqmn
    port map (lncyyu => mxumexs, h => wiz, uejcnlcc => zvstx, bocbeyq => bid);
  uaqqdbmuxv : entity work.saqmn
    port map (lncyyu => lmveeww, h => zrz, uejcnlcc => mm, bocbeyq => mdhwl);
  ca : entity work.saqmn
    port map (lncyyu => mxumexs, h => zrz, uejcnlcc => zvstx, bocbeyq => fqwvp);
  mtau : entity work.saqmn
    port map (lncyyu => yzqyc, h => wiz, uejcnlcc => zvstx, bocbeyq => imqijeoaa);
  
  -- Single-driven assignments
  mxumexs <= (TRUE, TRUE, FALSE, FALSE, TRUE);
  lmveeww <= yzqyc;
  pq <= (8#1455#, 0_0_2_1, 23031);
  
  -- Multi-driven assignments
  wiz <= wiz;
  wiz <= wiz;
  wiz <= (others => 'X');
end moyw;



-- Seed after: 15968924925481305322,14094562573555574003
