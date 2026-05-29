-- Seed: 10129556676576226527,16715549879197889543



entity wv is
  port (ositj : out real; fhfkz : inout real_vector(4 to 0); x : in integer_vector(3 downto 1); ogkst : linkage integer);
end wv;



architecture jzlq of wv is
  
begin
  
end jzlq;



entity z is
  port (qtctm : buffer time; i : in severity_level);
end z;



architecture vfofz of z is
  signal ecrxgz : integer;
  signal uxcvgn : integer_vector(3 downto 1);
  signal jqjxbi : real_vector(4 to 0);
  signal ohzey : real;
begin
  sf : entity work.wv
    port map (ositj => ohzey, fhfkz => jqjxbi, x => uxcvgn, ogkst => ecrxgz);
end vfofz;

library ieee;
use ieee.std_logic_1164.all;

entity cg is
  port (mgpyeqyaba : in std_logic_vector(0 to 2); mzykwdgcs : linkage std_logic_vector(4 downto 0); jnnxczcdyb : linkage time; yphwzjvr : in integer);
end cg;



architecture buziyxg of cg is
  signal g : integer;
  signal inzrmxweh : real_vector(4 to 0);
  signal odg : real;
  signal yxcremclew : severity_level;
  signal tjhwah : time;
  signal siftnzll : severity_level;
  signal v : time;
  signal adgjxsbbx : integer_vector(3 downto 1);
  signal oneuhc : real_vector(4 to 0);
  signal x : real;
begin
  lcbtpvsc : entity work.wv
    port map (ositj => x, fhfkz => oneuhc, x => adgjxsbbx, ogkst => yphwzjvr);
  ga : entity work.z
    port map (qtctm => v, i => siftnzll);
  kq : entity work.z
    port map (qtctm => tjhwah, i => yxcremclew);
  xbeeuv : entity work.wv
    port map (ositj => odg, fhfkz => inzrmxweh, x => adgjxsbbx, ogkst => g);
end buziyxg;

library ieee;
use ieee.std_logic_1164.all;

entity lf is
  port (ujmmsfxrl : out std_logic_vector(3 to 0); gx : inout integer);
end lf;



architecture kf of lf is
  
begin
  
end kf;



-- Seed after: 13202763759835800083,16715549879197889543
