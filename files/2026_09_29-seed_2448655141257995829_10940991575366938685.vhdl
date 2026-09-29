-- Seed: 2448655141257995829,10940991575366938685

library ieee;
use ieee.std_logic_1164.all;

entity gfdxwykm is
  port (dvkfpj : out std_logic_vector(0 to 3); uzhq : inout std_logic);
end gfdxwykm;

architecture ylzcqvqdz of gfdxwykm is
  
begin
  
end ylzcqvqdz;

entity vaer is
  port (yjwcvhmyx : linkage real; yklvig : inout integer);
end vaer;

library ieee;
use ieee.std_logic_1164.all;

architecture tdckgbbn of vaer is
  signal uvfup : std_logic;
  signal iur : std_logic;
  signal kpsgrhsod : std_logic_vector(0 to 3);
  signal ybzkop : std_logic;
  signal rvrkblkipb : std_logic_vector(0 to 3);
  signal agwqj : std_logic;
  signal xwcnpkw : std_logic_vector(0 to 3);
begin
  psvjmaq : entity work.gfdxwykm
    port map (dvkfpj => xwcnpkw, uzhq => agwqj);
  p : entity work.gfdxwykm
    port map (dvkfpj => rvrkblkipb, uzhq => ybzkop);
  bewzkv : entity work.gfdxwykm
    port map (dvkfpj => kpsgrhsod, uzhq => iur);
  eg : entity work.gfdxwykm
    port map (dvkfpj => xwcnpkw, uzhq => uvfup);
  
  -- Single-driven assignments
  yklvig <= 2_1;
  
  -- Multi-driven assignments
  iur <= uvfup;
  kpsgrhsod <= ('0', 'L', 'L', 'X');
end tdckgbbn;

library ieee;
use ieee.std_logic_1164.all;

entity oaprm is
  port (unnsgh : out std_logic_vector(1 to 2); qokgatfi : out std_logic; q : linkage integer_vector(4 to 3));
end oaprm;

library ieee;
use ieee.std_logic_1164.all;

architecture wlk of oaprm is
  signal ywjpl : std_logic_vector(0 to 3);
  signal gutwm : integer;
  signal fo : real;
  signal joooaj : std_logic;
  signal swjweybcaq : std_logic_vector(0 to 3);
begin
  jmvc : entity work.gfdxwykm
    port map (dvkfpj => swjweybcaq, uzhq => joooaj);
  kbi : entity work.vaer
    port map (yjwcvhmyx => fo, yklvig => gutwm);
  fbx : entity work.gfdxwykm
    port map (dvkfpj => swjweybcaq, uzhq => qokgatfi);
  uwuenkhuk : entity work.gfdxwykm
    port map (dvkfpj => ywjpl, uzhq => qokgatfi);
  
  -- Multi-driven assignments
  ywjpl <= swjweybcaq;
  joooaj <= qokgatfi;
  qokgatfi <= qokgatfi;
  joooaj <= 'X';
end wlk;

entity hvszaocg is
  port (uy : buffer bit; easgihd : in real);
end hvszaocg;

architecture sc of hvszaocg is
  
begin
  -- Single-driven assignments
  uy <= '0';
end sc;



-- Seed after: 4883445114524937027,10940991575366938685
