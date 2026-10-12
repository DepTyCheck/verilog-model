-- Seed: 13841592513957564356,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity hugdsa is
  port (t : inout real; rwgnctty : out time; pydhdgkv : out std_logic_vector(3 to 2));
end hugdsa;

architecture gor of hugdsa is
  
begin
  -- Single-driven assignments
  t <= t;
  rwgnctty <= rwgnctty;
  
  -- Multi-driven assignments
  pydhdgkv <= pydhdgkv;
  pydhdgkv <= pydhdgkv;
  pydhdgkv <= (others => '0');
end gor;

entity cimadkcw is
  port (ojarvtf : out time);
end cimadkcw;

library ieee;
use ieee.std_logic_1164.all;

architecture yjnsyojt of cimadkcw is
  signal jivk : real;
  signal zade : std_logic_vector(3 to 2);
  signal borhw : time;
  signal gjv : real;
  signal desrr : std_logic_vector(3 to 2);
  signal mmizbfhgwm : time;
  signal rth : real;
begin
  lmzgnqi : entity work.hugdsa
    port map (t => rth, rwgnctty => mmizbfhgwm, pydhdgkv => desrr);
  wkhuhpebvz : entity work.hugdsa
    port map (t => gjv, rwgnctty => borhw, pydhdgkv => zade);
  mzrp : entity work.hugdsa
    port map (t => jivk, rwgnctty => ojarvtf, pydhdgkv => desrr);
  
  -- Multi-driven assignments
  zade <= (others => '0');
end yjnsyojt;

library ieee;
use ieee.std_logic_1164.all;

entity pizixcvpdq is
  port (ynmwdfdsy : buffer real; lth : in time; quzhciiwac : out real; pbdvni : linkage std_logic);
end pizixcvpdq;

library ieee;
use ieee.std_logic_1164.all;

architecture kibthcy of pizixcvpdq is
  signal bxp : time;
  signal plysjlot : time;
  signal z : real;
  signal pwvlc : std_logic_vector(3 to 2);
  signal ghyukbngx : time;
begin
  mbqwwfrrd : entity work.hugdsa
    port map (t => quzhciiwac, rwgnctty => ghyukbngx, pydhdgkv => pwvlc);
  gosmwyrjn : entity work.hugdsa
    port map (t => z, rwgnctty => plysjlot, pydhdgkv => pwvlc);
  ncpcwg : entity work.cimadkcw
    port map (ojarvtf => bxp);
  
  -- Single-driven assignments
  ynmwdfdsy <= quzhciiwac;
  
  -- Multi-driven assignments
  pwvlc <= (others => '0');
  pwvlc <= (others => '0');
  pwvlc <= (others => '0');
  pwvlc <= "";
end kibthcy;



-- Seed after: 17192477160205765407,3426943171337919827
