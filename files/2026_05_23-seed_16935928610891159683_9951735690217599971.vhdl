-- Seed: 16935928610891159683,9951735690217599971



entity xll is
  port (kodk : in time; oerqr : in character; jrcggoftc : inout time; cbbyzfpx : inout time_vector(3 downto 3));
end xll;



architecture ggv of xll is
  
begin
  
end ggv;



entity celskf is
  port (mncqo : out time);
end celskf;



architecture znqe of celskf is
  signal g : time_vector(3 downto 3);
  signal lxxx : time;
  signal yzqxtcog : character;
  signal bumhji : time;
begin
  wklhmloho : entity work.xll
    port map (kodk => bumhji, oerqr => yzqxtcog, jrcggoftc => lxxx, cbbyzfpx => g);
end znqe;

library ieee;
use ieee.std_logic_1164.all;

entity klyx is
  port (nm : in time_vector(2 to 0); dpdt : buffer std_logic; qopxjlzh : out character);
end klyx;



architecture czt of klyx is
  signal wfjiw : time_vector(3 downto 3);
  signal qknrmnaetb : time;
  signal nfwbkrchgu : time;
  signal ubst : time;
  signal bwavmmvw : time_vector(3 downto 3);
  signal xessvwgcs : time;
begin
  fdrtgvhx : entity work.xll
    port map (kodk => xessvwgcs, oerqr => qopxjlzh, jrcggoftc => xessvwgcs, cbbyzfpx => bwavmmvw);
  wq : entity work.celskf
    port map (mncqo => ubst);
  xdcyjyvlg : entity work.celskf
    port map (mncqo => nfwbkrchgu);
  jyumirhpzq : entity work.xll
    port map (kodk => qknrmnaetb, oerqr => qopxjlzh, jrcggoftc => qknrmnaetb, cbbyzfpx => wfjiw);
end czt;



-- Seed after: 12432755252069212490,9951735690217599971
