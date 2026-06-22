-- Seed: 8162109978875651969,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity hrxhmz is
  port (bbriclvdw : buffer time_vector(4 to 4); btm : in std_logic_vector(3 downto 1); mx : buffer real; roairjuuo : in string(4 to 1));
end hrxhmz;

architecture mncytjsef of hrxhmz is
  
begin
  
end mncytjsef;

library ieee;
use ieee.std_logic_1164.all;

entity qa is
  port (mhethclldu : out std_logic_vector(4 to 3); ctexzezrvq : in std_logic);
end qa;

library ieee;
use ieee.std_logic_1164.all;

architecture fcn of qa is
  signal kwksrx : string(4 to 1);
  signal tfq : real;
  signal vgphtoedz : std_logic_vector(3 downto 1);
  signal tqjugwzw : time_vector(4 to 4);
begin
  ypkzzgjnaf : entity work.hrxhmz
    port map (bbriclvdw => tqjugwzw, btm => vgphtoedz, mx => tfq, roairjuuo => kwksrx);
  
  -- Single-driven assignments
  kwksrx <= "";
  
  -- Multi-driven assignments
  mhethclldu <= "";
  mhethclldu <= (others => '0');
end fcn;

entity rxtknxlkhx is
  port (xfkl : out integer; spppar : inout time);
end rxtknxlkhx;

library ieee;
use ieee.std_logic_1164.all;

architecture svuqoq of rxtknxlkhx is
  signal znvdphf : string(4 to 1);
  signal gb : real;
  signal eawlh : std_logic_vector(3 downto 1);
  signal lmsmeeeeqa : time_vector(4 to 4);
  signal kzkjcdwye : std_logic;
  signal mceypuay : std_logic_vector(4 to 3);
begin
  upcvbiy : entity work.qa
    port map (mhethclldu => mceypuay, ctexzezrvq => kzkjcdwye);
  oegr : entity work.hrxhmz
    port map (bbriclvdw => lmsmeeeeqa, btm => eawlh, mx => gb, roairjuuo => znvdphf);
  
  -- Multi-driven assignments
  mceypuay <= (others => '0');
  kzkjcdwye <= '0';
  eawlh <= ('1', 'Z', 'L');
end svuqoq;

library ieee;
use ieee.std_logic_1164.all;

entity ygwzxfbslh is
  port (ettzblw : linkage time_vector(2 downto 0); vc : buffer character; hkym : inout std_logic);
end ygwzxfbslh;

library ieee;
use ieee.std_logic_1164.all;

architecture amsoqnby of ygwzxfbslh is
  signal fjs : string(4 to 1);
  signal g : real;
  signal wpayfclbj : std_logic_vector(3 downto 1);
  signal krmgv : time_vector(4 to 4);
begin
  xp : entity work.hrxhmz
    port map (bbriclvdw => krmgv, btm => wpayfclbj, mx => g, roairjuuo => fjs);
  
  -- Single-driven assignments
  vc <= 'a';
  fjs <= "";
  
  -- Multi-driven assignments
  wpayfclbj <= "XL1";
end amsoqnby;



-- Seed after: 9363614133119886342,13479070923501788437
