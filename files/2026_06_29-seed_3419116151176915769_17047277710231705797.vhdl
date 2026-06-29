-- Seed: 3419116151176915769,17047277710231705797

entity borpyahxj is
  port (vzqyfqr : buffer integer_vector(3 to 0));
end borpyahxj;

architecture pgatsdc of borpyahxj is
  
begin
  -- Single-driven assignments
  vzqyfqr <= (others => 0);
end pgatsdc;

library ieee;
use ieee.std_logic_1164.all;

entity ptoimwdpvd is
  port (qhuwtopm : inout integer; g : out integer; nwr : linkage std_logic_vector(4 downto 1); cxuix : in std_logic_vector(3 to 0));
end ptoimwdpvd;

architecture mdoku of ptoimwdpvd is
  signal uyl : integer_vector(3 to 0);
  signal jauo : integer_vector(3 to 0);
  signal qiznhtikom : integer_vector(3 to 0);
begin
  bhnzdih : entity work.borpyahxj
    port map (vzqyfqr => qiznhtikom);
  yzdrikwfr : entity work.borpyahxj
    port map (vzqyfqr => jauo);
  ewwnzjj : entity work.borpyahxj
    port map (vzqyfqr => uyl);
  
  -- Single-driven assignments
  g <= 40124;
  qhuwtopm <= 3;
end mdoku;

library ieee;
use ieee.std_logic_1164.all;

entity rq is
  port (zayequhc : buffer std_logic_vector(0 to 3); y : buffer time);
end rq;

library ieee;
use ieee.std_logic_1164.all;

architecture uipprarams of rq is
  signal kc : std_logic_vector(3 to 0);
  signal ouerhx : integer;
  signal ivzhm : integer;
  signal ztpgz : integer_vector(3 to 0);
begin
  swqsy : entity work.borpyahxj
    port map (vzqyfqr => ztpgz);
  ycfaj : entity work.ptoimwdpvd
    port map (qhuwtopm => ivzhm, g => ouerhx, nwr => zayequhc, cxuix => kc);
  
  -- Multi-driven assignments
  zayequhc <= ('U', 'L', '0', '1');
  zayequhc <= ('1', 'W', 'W', '1');
  zayequhc <= ('X', 'X', 'L', 'X');
  zayequhc <= ('Z', '1', '1', 'L');
end uipprarams;



-- Seed after: 11486836486223527502,17047277710231705797
