-- Seed: 10928864585488850148,16159265764638711791

entity nkcqynr is
  port (kpgl : buffer time_vector(3 downto 3); aiu : buffer bit_vector(0 downto 0); yzhtflrst : out integer);
end nkcqynr;

architecture hqooqz of nkcqynr is
  
begin
  -- Single-driven assignments
  kpgl <= (others => 8#310# ns);
  yzhtflrst <= yzhtflrst;
end hqooqz;

entity pfhx is
  port (yvb : inout string(4 downto 3); lgbiw : in string(2 downto 3); sw : inout real_vector(0 to 1));
end pfhx;

architecture iqjtmeamw of pfhx is
  signal zzzpd : integer;
  signal njgyfvt : bit_vector(0 downto 0);
  signal n : time_vector(3 downto 3);
  signal b : integer;
  signal ky : bit_vector(0 downto 0);
  signal nhyxmvxcx : time_vector(3 downto 3);
  signal wdacgigt : integer;
  signal x : bit_vector(0 downto 0);
  signal mtb : time_vector(3 downto 3);
begin
  cusny : entity work.nkcqynr
    port map (kpgl => mtb, aiu => x, yzhtflrst => wdacgigt);
  eqvyrvfij : entity work.nkcqynr
    port map (kpgl => nhyxmvxcx, aiu => ky, yzhtflrst => b);
  osi : entity work.nkcqynr
    port map (kpgl => n, aiu => njgyfvt, yzhtflrst => zzzpd);
  
  -- Single-driven assignments
  sw <= sw;
  yvb <= yvb;
end iqjtmeamw;

entity rxgtacwvz is
  port (j : in real);
end rxgtacwvz;

architecture nmgnkr of rxgtacwvz is
  signal ofasscx : real_vector(0 to 1);
  signal iitik : string(2 downto 3);
  signal oqcbhbqwkg : string(4 downto 3);
  signal pglgonugnr : integer;
  signal z : bit_vector(0 downto 0);
  signal rwojg : time_vector(3 downto 3);
begin
  ios : entity work.nkcqynr
    port map (kpgl => rwojg, aiu => z, yzhtflrst => pglgonugnr);
  u : entity work.pfhx
    port map (yvb => oqcbhbqwkg, lgbiw => iitik, sw => ofasscx);
  
  -- Single-driven assignments
  iitik <= iitik;
end nmgnkr;

entity mqm is
  port (uwcjkucx : inout character; xnckykfxq : inout bit);
end mqm;

architecture xlhzwif of mqm is
  signal nx : integer;
  signal mmeooqymjs : bit_vector(0 downto 0);
  signal itkcbnow : time_vector(3 downto 3);
  signal lefuloe : integer;
  signal rh : bit_vector(0 downto 0);
  signal qse : time_vector(3 downto 3);
  signal bkzg : real;
begin
  xb : entity work.rxgtacwvz
    port map (j => bkzg);
  zwylnjjw : entity work.nkcqynr
    port map (kpgl => qse, aiu => rh, yzhtflrst => lefuloe);
  kpcoqapdr : entity work.rxgtacwvz
    port map (j => bkzg);
  eotecg : entity work.nkcqynr
    port map (kpgl => itkcbnow, aiu => mmeooqymjs, yzhtflrst => nx);
  
  -- Single-driven assignments
  xnckykfxq <= xnckykfxq;
  uwcjkucx <= 'c';
end xlhzwif;



-- Seed after: 10298035204578069281,16159265764638711791
