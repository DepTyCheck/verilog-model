-- Seed: 15918932267874114718,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity rde is
  port (ubjkb : in time; puyvup : in boolean_vector(4 to 0); zvmflawt : out std_logic_vector(0 to 3));
end rde;

architecture ikgoljpvw of rde is
  
begin
  -- Multi-driven assignments
  zvmflawt <= zvmflawt;
  zvmflawt <= "010L";
  zvmflawt <= "XXLZ";
end ikgoljpvw;

library ieee;
use ieee.std_logic_1164.all;

entity ztkakuyy is
  port (ogdxwdqnnv : buffer time; kg : inout std_logic_vector(3 downto 1); xms : in time);
end ztkakuyy;

architecture ju of ztkakuyy is
  
begin
  -- Single-driven assignments
  ogdxwdqnnv <= xms;
  
  -- Multi-driven assignments
  kg <= kg;
  kg <= "LHL";
end ju;

entity fyyaiq is
  port (cd : out time; vsgd : out time_vector(3 to 1));
end fyyaiq;

library ieee;
use ieee.std_logic_1164.all;

architecture wgjurbk of fyyaiq is
  signal l : std_logic_vector(3 downto 1);
  signal poqglx : std_logic_vector(0 to 3);
  signal dazlv : boolean_vector(4 to 0);
  signal unbsnfcab : time;
  signal tlxerpssby : time;
  signal pwctf : std_logic_vector(3 downto 1);
begin
  ikprrb : entity work.ztkakuyy
    port map (ogdxwdqnnv => cd, kg => pwctf, xms => tlxerpssby);
  xf : entity work.rde
    port map (ubjkb => unbsnfcab, puyvup => dazlv, zvmflawt => poqglx);
  rheeqvudq : entity work.ztkakuyy
    port map (ogdxwdqnnv => tlxerpssby, kg => l, xms => cd);
  
  -- Single-driven assignments
  vsgd <= vsgd;
  dazlv <= dazlv;
  
  -- Multi-driven assignments
  l <= ('W', 'L', '-');
end wgjurbk;

library ieee;
use ieee.std_logic_1164.all;

entity cr is
  port (amggr : buffer std_logic_vector(2 downto 1); vgv : out real_vector(1 downto 0); rqsxuz : buffer real);
end cr;

library ieee;
use ieee.std_logic_1164.all;

architecture qpdecirx of cr is
  signal pdbvh : std_logic_vector(0 to 3);
  signal jlslknslpe : boolean_vector(4 to 0);
  signal nnsygmhwev : time;
begin
  cmqfbyya : entity work.rde
    port map (ubjkb => nnsygmhwev, puyvup => jlslknslpe, zvmflawt => pdbvh);
  
  -- Multi-driven assignments
  pdbvh <= pdbvh;
  amggr <= "00";
  amggr <= ('Z', 'W');
  amggr <= "UZ";
end qpdecirx;



-- Seed after: 644852016236503719,8412319452373742525
