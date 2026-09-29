-- Seed: 13058874818775828957,10940991575366938685

entity qi is
  port (gmnjs : in character; kgychqg : in time_vector(3 downto 4));
end qi;

architecture s of qi is
  
begin
  
end s;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (xi : out time; tzun : inout std_logic_vector(2 to 1); jz : linkage time);
end d;

architecture knygwql of d is
  signal aeon : time_vector(3 downto 4);
  signal ohzkhkefli : character;
begin
  zydmhp : entity work.qi
    port map (gmnjs => ohzkhkefli, kgychqg => aeon);
  
  -- Single-driven assignments
  xi <= xi;
end knygwql;

entity hv is
  port (i : in integer; fxlixwrvwk : out real; zezns : in real; filke : out integer_vector(3 downto 1));
end hv;

library ieee;
use ieee.std_logic_1164.all;

architecture nbu of hv is
  signal tb : time;
  signal somwowmat : std_logic_vector(2 to 1);
  signal ltliixvj : time;
begin
  vyafkdzmse : entity work.d
    port map (xi => ltliixvj, tzun => somwowmat, jz => tb);
  
  -- Multi-driven assignments
  somwowmat <= "";
  somwowmat <= "";
  somwowmat <= somwowmat;
end nbu;

library ieee;
use ieee.std_logic_1164.all;

entity ir is
  port (vsqeydzhe : buffer std_logic; hsslxuw : inout boolean; utpnf : linkage bit; o : inout integer);
end ir;

library ieee;
use ieee.std_logic_1164.all;

architecture mn of ir is
  signal pfbnqu : time_vector(3 downto 4);
  signal cvrewthzd : time_vector(3 downto 4);
  signal vcayeliogj : character;
  signal zfbxqxqqgg : integer_vector(3 downto 1);
  signal hofyfk : real;
  signal wvl : time;
  signal uqi : std_logic_vector(2 to 1);
  signal jenbnhl : time;
begin
  h : entity work.d
    port map (xi => jenbnhl, tzun => uqi, jz => wvl);
  eocejcvati : entity work.hv
    port map (i => o, fxlixwrvwk => hofyfk, zezns => hofyfk, filke => zfbxqxqqgg);
  zunyje : entity work.qi
    port map (gmnjs => vcayeliogj, kgychqg => cvrewthzd);
  lmusny : entity work.qi
    port map (gmnjs => vcayeliogj, kgychqg => pfbnqu);
  
  -- Single-driven assignments
  o <= o;
  
  -- Multi-driven assignments
  vsqeydzhe <= vsqeydzhe;
  vsqeydzhe <= 'X';
  vsqeydzhe <= 'U';
  uqi <= "";
end mn;



-- Seed after: 17873823068511034953,10940991575366938685
