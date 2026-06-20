-- Seed: 8185087544365087345,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity wtsvh is
  port (vnvib : inout std_logic; qx : inout time);
end wtsvh;

architecture zf of wtsvh is
  
begin
  -- Single-driven assignments
  qx <= 1 hr;
  
  -- Multi-driven assignments
  vnvib <= 'X';
end zf;

library ieee;
use ieee.std_logic_1164.all;

entity del is
  port ( ckadufjizb : inout time
  ; qakrir : buffer severity_level
  ; fnlmrpo : linkage boolean_vector(3 downto 4)
  ; ttluotreaf : out std_logic_vector(3 downto 3)
  );
end del;

architecture ere of del is
  
begin
  -- Single-driven assignments
  ckadufjizb <= 8#75# fs;
  
  -- Multi-driven assignments
  ttluotreaf <= (others => 'H');
  ttluotreaf <= (others => 'U');
end ere;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (wxgybllh : out real; ng : inout integer; ucm : in std_logic);
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture mdmizpfc of s is
  signal k : time;
  signal ktgjrmvrk : std_logic;
  signal npsy : std_logic_vector(3 downto 3);
  signal eqfbcbo : boolean_vector(3 downto 4);
  signal rhcqokibd : severity_level;
  signal m : time;
begin
  y : entity work.del
    port map (ckadufjizb => m, qakrir => rhcqokibd, fnlmrpo => eqfbcbo, ttluotreaf => npsy);
  qru : entity work.wtsvh
    port map (vnvib => ktgjrmvrk, qx => k);
  
  -- Single-driven assignments
  ng <= 2#1_1_1#;
  wxgybllh <= 1_3_2_3_3.2_3_0_1;
  
  -- Multi-driven assignments
  ktgjrmvrk <= '-';
  npsy <= (others => '0');
end mdmizpfc;

library ieee;
use ieee.std_logic_1164.all;

entity ehrdv is
  port (zbntcpw : inout std_logic; iod : buffer std_logic);
end ehrdv;

library ieee;
use ieee.std_logic_1164.all;

architecture ork of ehrdv is
  signal ciowwltt : std_logic;
  signal i : integer;
  signal rnfnlnldv : real;
  signal zqwdhx : boolean_vector(3 downto 4);
  signal wnfuuloi : severity_level;
  signal eu : time;
  signal zoninzbpfi : std_logic_vector(3 downto 3);
  signal g : boolean_vector(3 downto 4);
  signal dyy : severity_level;
  signal vyfdmg : time;
begin
  wpvpfmdx : entity work.del
    port map (ckadufjizb => vyfdmg, qakrir => dyy, fnlmrpo => g, ttluotreaf => zoninzbpfi);
  gswwyaw : entity work.del
    port map (ckadufjizb => eu, qakrir => wnfuuloi, fnlmrpo => zqwdhx, ttluotreaf => zoninzbpfi);
  cfkz : entity work.s
    port map (wxgybllh => rnfnlnldv, ng => i, ucm => ciowwltt);
end ork;



-- Seed after: 7269364427851869415,3924983747739634027
