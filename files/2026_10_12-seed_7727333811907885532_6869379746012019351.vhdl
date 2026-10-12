-- Seed: 7727333811907885532,6869379746012019351

entity ncvmicam is
  port (njsgcom : out time; quj : inout character);
end ncvmicam;

architecture vxi of ncvmicam is
  
begin
  -- Single-driven assignments
  quj <= 'o';
  njsgcom <= 4 sec;
end vxi;

library ieee;
use ieee.std_logic_1164.all;

entity ys is
  port (ssadpe : out time; uk : out bit; kusqtfohd : inout std_logic_vector(2 to 0));
end ys;

architecture khwrngmf of ys is
  signal kxbdkbbinn : character;
  signal mvxyqptfm : time;
  signal frixycakt : character;
begin
  gnp : entity work.ncvmicam
    port map (njsgcom => ssadpe, quj => frixycakt);
  thk : entity work.ncvmicam
    port map (njsgcom => mvxyqptfm, quj => kxbdkbbinn);
  
  -- Single-driven assignments
  uk <= uk;
  
  -- Multi-driven assignments
  kusqtfohd <= "";
  kusqtfohd <= (others => '0');
  kusqtfohd <= "";
end khwrngmf;

library ieee;
use ieee.std_logic_1164.all;

entity rtmritui is
  port ( aaqpxblffx : inout time
  ; bsuzkas : inout severity_level
  ; ufinuej : buffer std_logic_vector(0 downto 0)
  ; avevcexe : in std_logic_vector(3 downto 0)
  );
end rtmritui;

architecture unokuyk of rtmritui is
  signal mwsezxq : character;
  signal xc : character;
  signal tpmkjwsecj : time;
  signal syjiizp : character;
  signal lddjhgs : time;
begin
  ovgstdagb : entity work.ncvmicam
    port map (njsgcom => lddjhgs, quj => syjiizp);
  snqyesgasx : entity work.ncvmicam
    port map (njsgcom => tpmkjwsecj, quj => xc);
  spxxjnwy : entity work.ncvmicam
    port map (njsgcom => aaqpxblffx, quj => mwsezxq);
  
  -- Single-driven assignments
  bsuzkas <= WARNING;
  
  -- Multi-driven assignments
  ufinuej <= ufinuej;
  ufinuej <= ufinuej;
end unokuyk;



-- Seed after: 18238222736965712893,6869379746012019351
