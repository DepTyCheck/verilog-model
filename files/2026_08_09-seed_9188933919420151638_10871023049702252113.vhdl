-- Seed: 9188933919420151638,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port ( kdw : buffer std_logic_vector(2 to 3)
  ; cfsrs : inout std_logic_vector(4 to 3)
  ; rcuqiobtst : inout std_logic
  ; m : linkage std_logic_vector(2 downto 3)
  );
end s;

architecture sqbg of s is
  
begin
  -- Multi-driven assignments
  rcuqiobtst <= rcuqiobtst;
  cfsrs <= (others => '0');
  kdw <= kdw;
  rcuqiobtst <= rcuqiobtst;
end sqbg;

library ieee;
use ieee.std_logic_1164.all;

entity cbvdswrh is
  port (yyxtno : inout std_logic);
end cbvdswrh;

library ieee;
use ieee.std_logic_1164.all;

architecture unkxvgp of cbvdswrh is
  signal jvtcvhjx : std_logic_vector(2 downto 3);
  signal zdmsi : std_logic;
  signal ka : std_logic_vector(4 to 3);
  signal glmapsocnr : std_logic_vector(2 to 3);
  signal rapfk : std_logic_vector(2 downto 3);
  signal zcbhdqijc : std_logic_vector(4 to 3);
  signal cbyqxygco : std_logic_vector(2 to 3);
  signal x : std_logic_vector(2 downto 3);
  signal dmeehdz : std_logic;
  signal fr : std_logic_vector(4 to 3);
  signal gcttiwtyh : std_logic_vector(2 to 3);
begin
  tvyhukq : entity work.s
    port map (kdw => gcttiwtyh, cfsrs => fr, rcuqiobtst => dmeehdz, m => x);
  mupboijqfk : entity work.s
    port map (kdw => cbyqxygco, cfsrs => zcbhdqijc, rcuqiobtst => yyxtno, m => rapfk);
  bslold : entity work.s
    port map (kdw => glmapsocnr, cfsrs => ka, rcuqiobtst => zdmsi, m => jvtcvhjx);
  
  -- Multi-driven assignments
  yyxtno <= yyxtno;
  jvtcvhjx <= jvtcvhjx;
  ka <= x;
end unkxvgp;

entity fxml is
  port (hetoksn : inout integer);
end fxml;

library ieee;
use ieee.std_logic_1164.all;

architecture x of fxml is
  signal xfb : std_logic_vector(2 downto 3);
  signal m : std_logic;
  signal lqz : std_logic_vector(4 to 3);
  signal zcwcrz : std_logic_vector(2 to 3);
begin
  n : entity work.s
    port map (kdw => zcwcrz, cfsrs => lqz, rcuqiobtst => m, m => xfb);
  fhyunm : entity work.cbvdswrh
    port map (yyxtno => m);
  
  -- Single-driven assignments
  hetoksn <= hetoksn;
  
  -- Multi-driven assignments
  zcwcrz <= "LW";
  zcwcrz <= ('0', 'W');
end x;



-- Seed after: 10977638273830563311,10871023049702252113
