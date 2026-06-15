-- Seed: 12210197870220288678,1834764876137802293

entity eeowxrnftd is
  port (uzyfcvbxz : inout time; nzvrjbrr : linkage integer; echdcjyrld : out time_vector(0 downto 3));
end eeowxrnftd;

architecture ybz of eeowxrnftd is
  
begin
  -- Single-driven assignments
  echdcjyrld <= (others => 0 ns);
  uzyfcvbxz <= 16#69.1# fs;
end ybz;

entity trsxe is
  port (cwhcz : out real_vector(4 to 3); dfrufulyiu : inout real_vector(1 to 0));
end trsxe;

architecture rbyqw of trsxe is
  signal bubt : time_vector(0 downto 3);
  signal tqcjmexk : integer;
  signal zzoiimlk : time;
  signal wbxflvfqu : time_vector(0 downto 3);
  signal yxjdzoacij : integer;
  signal rn : time;
  signal erf : time_vector(0 downto 3);
  signal npetwa : integer;
  signal kuvazgd : time;
  signal i : time_vector(0 downto 3);
  signal vegcuo : integer;
  signal fdtou : time;
begin
  xsguaxg : entity work.eeowxrnftd
    port map (uzyfcvbxz => fdtou, nzvrjbrr => vegcuo, echdcjyrld => i);
  tvqbeseql : entity work.eeowxrnftd
    port map (uzyfcvbxz => kuvazgd, nzvrjbrr => npetwa, echdcjyrld => erf);
  cuf : entity work.eeowxrnftd
    port map (uzyfcvbxz => rn, nzvrjbrr => yxjdzoacij, echdcjyrld => wbxflvfqu);
  grnmpdgec : entity work.eeowxrnftd
    port map (uzyfcvbxz => zzoiimlk, nzvrjbrr => tqcjmexk, echdcjyrld => bubt);
  
  -- Single-driven assignments
  cwhcz <= (others => 0.0);
  dfrufulyiu <= (others => 0.0);
end rbyqw;

entity cws is
  port (vsvtra : linkage boolean_vector(4 downto 1));
end cws;

architecture udco of cws is
  signal olhg : time_vector(0 downto 3);
  signal xndylzew : integer;
  signal xnt : time;
  signal b : real_vector(1 to 0);
  signal fcszttvnbe : real_vector(4 to 3);
  signal q : real_vector(1 to 0);
  signal wiyc : real_vector(4 to 3);
  signal tbdiahogct : real_vector(1 to 0);
  signal uxpnmpe : real_vector(4 to 3);
begin
  yc : entity work.trsxe
    port map (cwhcz => uxpnmpe, dfrufulyiu => tbdiahogct);
  vompptpap : entity work.trsxe
    port map (cwhcz => wiyc, dfrufulyiu => q);
  jovxrcdwr : entity work.trsxe
    port map (cwhcz => fcszttvnbe, dfrufulyiu => b);
  yowtmebz : entity work.eeowxrnftd
    port map (uzyfcvbxz => xnt, nzvrjbrr => xndylzew, echdcjyrld => olhg);
end udco;



-- Seed after: 6528740067190575378,1834764876137802293
