-- Seed: 2122379846755648720,511364357853360275

entity k is
  port (uaq : inout character; eeod : inout real; nlp : linkage time_vector(2 downto 2); vay : out real_vector(3 to 3));
end k;

architecture brxt of k is
  
begin
  -- Single-driven assignments
  vay <= vay;
  uaq <= 'e';
  eeod <= 16#3_4_5_F_4.B_B#;
end brxt;

entity lduliijxvu is
  port (rcfxphqif : in real; hbpshlx : linkage real; imngfzovb : in integer; cafm : inout time);
end lduliijxvu;

architecture mpuog of lduliijxvu is
  signal kgaj : real_vector(3 to 3);
  signal q : time_vector(2 downto 2);
  signal hroyknz : real;
  signal fnmcvxwhk : character;
  signal d : real_vector(3 to 3);
  signal bsmtg : time_vector(2 downto 2);
  signal phgheat : real;
  signal hls : character;
  signal jkitmyck : real_vector(3 to 3);
  signal cfyubkqxpa : time_vector(2 downto 2);
  signal atsfzqor : real;
  signal yobrgwntql : character;
begin
  nbx : entity work.k
    port map (uaq => yobrgwntql, eeod => atsfzqor, nlp => cfyubkqxpa, vay => jkitmyck);
  s : entity work.k
    port map (uaq => hls, eeod => phgheat, nlp => bsmtg, vay => d);
  hkffs : entity work.k
    port map (uaq => fnmcvxwhk, eeod => hroyknz, nlp => q, vay => kgaj);
end mpuog;

entity pyhtebcqe is
  port (gpkls : out character);
end pyhtebcqe;

architecture mlwvsxih of pyhtebcqe is
  signal grdftxgypd : time;
  signal bwj : integer;
  signal dijiyugr : real;
  signal pqpfts : real_vector(3 to 3);
  signal hejdjuvx : time_vector(2 downto 2);
  signal zxkizomebf : real;
begin
  dudnck : entity work.k
    port map (uaq => gpkls, eeod => zxkizomebf, nlp => hejdjuvx, vay => pqpfts);
  db : entity work.lduliijxvu
    port map (rcfxphqif => dijiyugr, hbpshlx => dijiyugr, imngfzovb => bwj, cafm => grdftxgypd);
end mlwvsxih;



-- Seed after: 10467278988769754352,511364357853360275
