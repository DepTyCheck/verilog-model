-- Seed: 9341975736103278966,12359743974512393525

entity qnkp is
  port (mrqv : out boolean_vector(2 downto 0));
end qnkp;

architecture czqmxkw of qnkp is
  
begin
  -- Single-driven assignments
  mrqv <= mrqv;
end czqmxkw;

library ieee;
use ieee.std_logic_1164.all;

entity fzrb is
  port (qssrp : linkage real; syyl : in std_logic);
end fzrb;

architecture cmov of fzrb is
  signal xul : boolean_vector(2 downto 0);
  signal woslrdw : boolean_vector(2 downto 0);
  signal fa : boolean_vector(2 downto 0);
begin
  wgv : entity work.qnkp
    port map (mrqv => fa);
  fwz : entity work.qnkp
    port map (mrqv => woslrdw);
  zmmhtao : entity work.qnkp
    port map (mrqv => xul);
end cmov;

library ieee;
use ieee.std_logic_1164.all;

entity wyoudmmgpw is
  port (aosecuj : buffer severity_level; moainjy : out std_logic_vector(2 to 3));
end wyoudmmgpw;

library ieee;
use ieee.std_logic_1164.all;

architecture ara of wyoudmmgpw is
  signal awga : boolean_vector(2 downto 0);
  signal eqsjjvfwi : boolean_vector(2 downto 0);
  signal xsvtstv : std_logic;
  signal tr : real;
begin
  ozgw : entity work.fzrb
    port map (qssrp => tr, syyl => xsvtstv);
  nqutu : entity work.qnkp
    port map (mrqv => eqsjjvfwi);
  eycffsy : entity work.qnkp
    port map (mrqv => awga);
  
  -- Single-driven assignments
  aosecuj <= WARNING;
  
  -- Multi-driven assignments
  moainjy <= ('L', 'W');
  moainjy <= moainjy;
  xsvtstv <= '0';
end ara;

library ieee;
use ieee.std_logic_1164.all;

entity pvanq is
  port (kch : linkage std_logic_vector(4 to 3));
end pvanq;

library ieee;
use ieee.std_logic_1164.all;

architecture bvd of pvanq is
  signal f : boolean_vector(2 downto 0);
  signal kgvyxqc : std_logic_vector(2 to 3);
  signal tnkhey : severity_level;
  signal bsgwv : boolean_vector(2 downto 0);
  signal apwymt : std_logic;
  signal h : real;
begin
  mqzspoylr : entity work.fzrb
    port map (qssrp => h, syyl => apwymt);
  vwon : entity work.qnkp
    port map (mrqv => bsgwv);
  mascci : entity work.wyoudmmgpw
    port map (aosecuj => tnkhey, moainjy => kgvyxqc);
  zyfvyhrony : entity work.qnkp
    port map (mrqv => f);
end bvd;



-- Seed after: 10717920940076466864,12359743974512393525
