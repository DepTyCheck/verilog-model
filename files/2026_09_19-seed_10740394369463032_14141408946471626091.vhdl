-- Seed: 10740394369463032,14141408946471626091

entity vrdnau is
  port (jc : buffer real; g : linkage integer_vector(1 to 4); o : buffer time);
end vrdnau;

architecture gh of vrdnau is
  
begin
  -- Single-driven assignments
  o <= 1 min;
end gh;

entity cpbcfvfqjn is
  port (wredajft : inout real);
end cpbcfvfqjn;

architecture ujjnhsubr of cpbcfvfqjn is
  signal fuxg : time;
  signal ecogapbxw : integer_vector(1 to 4);
  signal cwc : time;
  signal yqaztsjh : integer_vector(1 to 4);
  signal g : real;
begin
  pbribwy : entity work.vrdnau
    port map (jc => g, g => yqaztsjh, o => cwc);
  kw : entity work.vrdnau
    port map (jc => wredajft, g => ecogapbxw, o => fuxg);
end ujjnhsubr;

entity awizhoyb is
  port (vslghnsw : in bit_vector(0 downto 3); ua : buffer real; fcp : inout real; lxnjad : out severity_level);
end awizhoyb;

architecture nxpsp of awizhoyb is
  signal xebpg : time;
  signal oktwgxxab : integer_vector(1 to 4);
  signal njy : real;
  signal fzp : time;
  signal zagvxtygxl : integer_vector(1 to 4);
  signal kso : real;
  signal pknezjmz : time;
  signal lqf : integer_vector(1 to 4);
  signal cnulamig : real;
begin
  ex : entity work.cpbcfvfqjn
    port map (wredajft => cnulamig);
  nv : entity work.vrdnau
    port map (jc => fcp, g => lqf, o => pknezjmz);
  ggqtyikf : entity work.vrdnau
    port map (jc => kso, g => zagvxtygxl, o => fzp);
  xlvvi : entity work.vrdnau
    port map (jc => njy, g => oktwgxxab, o => xebpg);
  
  -- Single-driven assignments
  lxnjad <= lxnjad;
  ua <= 16#CF91E.3F0ED#;
end nxpsp;



-- Seed after: 7029782697472018732,14141408946471626091
