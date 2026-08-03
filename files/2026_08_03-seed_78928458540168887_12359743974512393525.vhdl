-- Seed: 78928458540168887,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity gvurx is
  port (g : in std_logic_vector(2 downto 3); vrxgstrxr : buffer severity_level);
end gvurx;

architecture scgaekz of gvurx is
  
begin
  -- Single-driven assignments
  vrxgstrxr <= NOTE;
end scgaekz;

library ieee;
use ieee.std_logic_1164.all;

entity bdm is
  port (tefk : inout bit; pmosu : inout std_logic);
end bdm;

library ieee;
use ieee.std_logic_1164.all;

architecture ilnybehew of bdm is
  signal zzvzuwglx : severity_level;
  signal knno : severity_level;
  signal ahkdmumoep : std_logic_vector(2 downto 3);
  signal k : severity_level;
  signal qtixej : std_logic_vector(2 downto 3);
begin
  unt : entity work.gvurx
    port map (g => qtixej, vrxgstrxr => k);
  pt : entity work.gvurx
    port map (g => ahkdmumoep, vrxgstrxr => knno);
  eifkh : entity work.gvurx
    port map (g => qtixej, vrxgstrxr => zzvzuwglx);
  
  -- Single-driven assignments
  tefk <= tefk;
  
  -- Multi-driven assignments
  pmosu <= 'H';
  pmosu <= 'X';
  ahkdmumoep <= (others => '0');
end ilnybehew;

entity mp is
  port (dvemynpf : out real; ysh : buffer time; hitcec : linkage integer);
end mp;

architecture vrfclwyj of mp is
  
begin
  -- Single-driven assignments
  ysh <= 2 ns;
  dvemynpf <= dvemynpf;
end vrfclwyj;

entity w is
  port (xwpeyhqib : inout time; fymwulayok : out time);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture och of w is
  signal lmltvul : severity_level;
  signal ci : std_logic_vector(2 downto 3);
  signal z : severity_level;
  signal yrdcht : std_logic_vector(2 downto 3);
  signal wa : integer;
  signal xbbc : real;
begin
  vljhlxl : entity work.mp
    port map (dvemynpf => xbbc, ysh => xwpeyhqib, hitcec => wa);
  lgcjnf : entity work.gvurx
    port map (g => yrdcht, vrxgstrxr => z);
  yiqvpknf : entity work.gvurx
    port map (g => ci, vrxgstrxr => lmltvul);
  
  -- Single-driven assignments
  fymwulayok <= fymwulayok;
end och;



-- Seed after: 2437550117861531094,12359743974512393525
