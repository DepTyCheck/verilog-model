-- Seed: 2009444716705639385,2511821214772927453

entity xmewxn is
  port (jlyus : inout real);
end xmewxn;

architecture ymrhvz of xmewxn is
  
begin
  -- Single-driven assignments
  jlyus <= 2.1_1;
end ymrhvz;

entity dcwjsx is
  port (vmjxkbjd : inout time);
end dcwjsx;

architecture xzjystccax of dcwjsx is
  signal loaweyqk : real;
  signal ajhour : real;
  signal ledhzui : real;
begin
  uk : entity work.xmewxn
    port map (jlyus => ledhzui);
  vjrars : entity work.xmewxn
    port map (jlyus => ajhour);
  bsuvlp : entity work.xmewxn
    port map (jlyus => loaweyqk);
end xzjystccax;

entity dk is
  port (tln : inout real; ehkfpvuf : buffer time; efgl : buffer integer);
end dk;

architecture jhxvhzzpwf of dk is
  
begin
  usytx : entity work.xmewxn
    port map (jlyus => tln);
  
  -- Single-driven assignments
  ehkfpvuf <= 041 ms;
  efgl <= efgl;
end jhxvhzzpwf;

library ieee;
use ieee.std_logic_1164.all;

entity otogi is
  port (r : inout std_logic_vector(1 to 2));
end otogi;

architecture lnewzgs of otogi is
  signal lism : time;
  signal ity : integer;
  signal yvjoxskw : time;
  signal zujgsvjud : real;
  signal ddbnn : time;
begin
  g : entity work.dcwjsx
    port map (vmjxkbjd => ddbnn);
  ibn : entity work.dk
    port map (tln => zujgsvjud, ehkfpvuf => yvjoxskw, efgl => ity);
  ujxqivcwj : entity work.dcwjsx
    port map (vmjxkbjd => lism);
  
  -- Multi-driven assignments
  r <= r;
end lnewzgs;



-- Seed after: 76234896426966167,2511821214772927453
