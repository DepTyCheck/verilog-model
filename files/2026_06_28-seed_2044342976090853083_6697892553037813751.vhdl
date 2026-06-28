-- Seed: 2044342976090853083,6697892553037813751

entity dhpp is
  port (zzmoflbed : out integer);
end dhpp;

architecture wrgbiw of dhpp is
  
begin
  
end wrgbiw;

entity kn is
  port (qqv : out integer; l : linkage real; vcsvejvgrs : linkage integer; mtqcvmcpru : inout time);
end kn;

architecture wvychwt of kn is
  signal cgwv : integer;
  signal ebpem : integer;
  signal ozxe : integer;
begin
  zjueg : entity work.dhpp
    port map (zzmoflbed => qqv);
  pqdvnhbflz : entity work.dhpp
    port map (zzmoflbed => ozxe);
  padnokxzv : entity work.dhpp
    port map (zzmoflbed => ebpem);
  s : entity work.dhpp
    port map (zzmoflbed => cgwv);
end wvychwt;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (fl : in std_logic; qlwjuwibs : linkage real; ouwnoxej : in std_logic_vector(1 downto 0));
end t;

architecture qx of t is
  signal rprglhtrpf : time;
  signal fkb : integer;
  signal ujshzrrw : integer;
  signal owzqjgx : time;
  signal nlh : integer;
  signal ircw : real;
  signal hfpq : integer;
  signal bvkgnsvhpl : time;
  signal gcppf : integer;
  signal zk : real;
  signal vsdikwylro : integer;
  signal ztfjuemrqx : integer;
begin
  vzawtpbr : entity work.dhpp
    port map (zzmoflbed => ztfjuemrqx);
  azveorel : entity work.kn
    port map (qqv => vsdikwylro, l => zk, vcsvejvgrs => gcppf, mtqcvmcpru => bvkgnsvhpl);
  rhaqvybaj : entity work.kn
    port map (qqv => hfpq, l => ircw, vcsvejvgrs => nlh, mtqcvmcpru => owzqjgx);
  kjgqn : entity work.kn
    port map (qqv => ujshzrrw, l => qlwjuwibs, vcsvejvgrs => fkb, mtqcvmcpru => rprglhtrpf);
end qx;



-- Seed after: 1794122023313686315,6697892553037813751
