-- Seed: 14404617878102835143,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (sszfhx : inout std_logic; ngmfurn : inout time; ofjakzm : linkage severity_level; jvqhysng : in std_logic_vector(3 to 3));
end x;

architecture qf of x is
  
begin
  -- Multi-driven assignments
  sszfhx <= '1';
  sszfhx <= 'L';
  sszfhx <= 'Z';
end qf;

entity ivwpwjiv is
  port (rkducqgr : inout bit_vector(1 to 0); mxf : inout integer; uz : out boolean; rnwydsfs : linkage integer);
end ivwpwjiv;

library ieee;
use ieee.std_logic_1164.all;

architecture wq of ivwpwjiv is
  signal nrhdnbx : severity_level;
  signal kdwwgmik : time;
  signal zezfthky : std_logic;
  signal jxv : severity_level;
  signal vuqcjfoe : time;
  signal hgxpdpvw : severity_level;
  signal ruv : time;
  signal xyyuawt : std_logic;
  signal vcdjfadb : std_logic_vector(3 to 3);
  signal pftrbnb : severity_level;
  signal odbnpk : time;
  signal dryo : std_logic;
begin
  i : entity work.x
    port map (sszfhx => dryo, ngmfurn => odbnpk, ofjakzm => pftrbnb, jvqhysng => vcdjfadb);
  ktilsjuvw : entity work.x
    port map (sszfhx => xyyuawt, ngmfurn => ruv, ofjakzm => hgxpdpvw, jvqhysng => vcdjfadb);
  hoz : entity work.x
    port map (sszfhx => dryo, ngmfurn => vuqcjfoe, ofjakzm => jxv, jvqhysng => vcdjfadb);
  ys : entity work.x
    port map (sszfhx => zezfthky, ngmfurn => kdwwgmik, ofjakzm => nrhdnbx, jvqhysng => vcdjfadb);
  
  -- Single-driven assignments
  uz <= TRUE;
  
  -- Multi-driven assignments
  dryo <= '-';
  xyyuawt <= 'U';
end wq;



-- Seed after: 12094690798695788779,13479070923501788437
