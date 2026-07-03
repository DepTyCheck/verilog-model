-- Seed: 5714342630448569293,2158184632809654795

use std.reflection.all;

entity begx is
  port (xswvelzvm : inout floating_value_mirror; asmnktnb : inout integer; xylewy : linkage integer; hotruxzdb : linkage character);
end begx;

architecture s of begx is
  
begin
  -- Single-driven assignments
  asmnktnb <= 2#0_1_0_1#;
end s;

entity ivnimrnm is
  port (ldprpk : inout integer_vector(2 downto 3));
end ivnimrnm;

use std.reflection.all;

architecture yjjqcg of ivnimrnm is
  signal h : character;
  signal kuqx : integer;
  signal dxmeisx : integer;
  shared variable u : floating_value_mirror;
  signal wphevrbr : character;
  signal lafwr : integer;
  signal mqqnia : integer;
  shared variable d : floating_value_mirror;
  signal gwe : character;
  signal amcwrdolb : integer;
  signal hsvfgrbl : integer;
  shared variable zxwkjocp : floating_value_mirror;
begin
  wijwv : entity work.begx
    port map (xswvelzvm => zxwkjocp, asmnktnb => hsvfgrbl, xylewy => amcwrdolb, hotruxzdb => gwe);
  lf : entity work.begx
    port map (xswvelzvm => d, asmnktnb => mqqnia, xylewy => lafwr, hotruxzdb => wphevrbr);
  xcbt : entity work.begx
    port map (xswvelzvm => u, asmnktnb => dxmeisx, xylewy => kuqx, hotruxzdb => h);
end yjjqcg;

use std.reflection.all;

entity ajnh is
  port (zq : out character; srj : buffer integer; vgpwkknc : inout record_subtype_mirror; yhwx : linkage real);
end ajnh;

use std.reflection.all;

architecture gtw of ajnh is
  signal uhobzgh : character;
  signal afxlbgjbd : integer;
  shared variable dx : floating_value_mirror;
  signal ovxvxdg : character;
  signal jb : integer;
  signal cncz : integer;
  shared variable jbyi : floating_value_mirror;
  signal ruuy : integer;
  signal tloobikkh : integer;
  shared variable uhbu : floating_value_mirror;
begin
  qi : entity work.begx
    port map (xswvelzvm => uhbu, asmnktnb => tloobikkh, xylewy => ruuy, hotruxzdb => zq);
  ggtfoan : entity work.begx
    port map (xswvelzvm => jbyi, asmnktnb => cncz, xylewy => jb, hotruxzdb => ovxvxdg);
  elahfqg : entity work.begx
    port map (xswvelzvm => dx, asmnktnb => srj, xylewy => afxlbgjbd, hotruxzdb => uhobzgh);
end gtw;

use std.reflection.all;

entity njv is
  port (e : inout value_mirror; zaesh : buffer time; lfieltm : buffer boolean_vector(0 downto 2); ismitxaz : inout integer_value_mirror);
end njv;

architecture pimnmxbaa of njv is
  signal i : integer_vector(2 downto 3);
begin
  fn : entity work.ivnimrnm
    port map (ldprpk => i);
  
  -- Single-driven assignments
  lfieltm <= (others => TRUE);
  zaesh <= 4 sec;
end pimnmxbaa;



-- Seed after: 11507604785097187330,2158184632809654795
