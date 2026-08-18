-- Seed: 5317206100023508433,5983430343285687595

entity fz is
  port (piryvyfdvy : out real_vector(4 to 1); lylwegkq : out boolean_vector(0 to 2));
end fz;

architecture impqtpx of fz is
  
begin
  -- Single-driven assignments
  lylwegkq <= (TRUE, FALSE, TRUE);
  piryvyfdvy <= (others => 0.0);
end impqtpx;

entity t is
  port (malp : in boolean; vorawr : inout integer; atry : inout real);
end t;

architecture oml of t is
  signal kpqsub : boolean_vector(0 to 2);
  signal fzsry : real_vector(4 to 1);
begin
  ztgwjzloy : entity work.fz
    port map (piryvyfdvy => fzsry, lylwegkq => kpqsub);
  
  -- Single-driven assignments
  atry <= 4.4_3;
  vorawr <= vorawr;
end oml;

entity yjscueu is
  port (ty : buffer time; c : out integer);
end yjscueu;

architecture pr of yjscueu is
  signal ybbx : boolean_vector(0 to 2);
  signal vr : real_vector(4 to 1);
  signal nfzviwrk : real;
  signal nhzdm : integer;
  signal moulefi : boolean;
  signal fsvxe : real;
  signal jbjkl : integer;
  signal fpmqc : boolean;
begin
  s : entity work.t
    port map (malp => fpmqc, vorawr => jbjkl, atry => fsvxe);
  uxssaavi : entity work.t
    port map (malp => moulefi, vorawr => nhzdm, atry => nfzviwrk);
  kfox : entity work.fz
    port map (piryvyfdvy => vr, lylwegkq => ybbx);
end pr;

entity dmy is
  port (ieisvj : out time_vector(4 downto 2); xm : linkage integer; e : out boolean_vector(4 to 0); jl : linkage time);
end dmy;

architecture dsmepx of dmy is
  signal odsqzht : real;
  signal fswgbvyyc : integer;
  signal yfd : boolean_vector(0 to 2);
  signal qear : real_vector(4 to 1);
  signal vje : real;
  signal myoshxpnp : integer;
  signal ftlppmat : boolean;
  signal ptwwowc : integer;
  signal ranoi : time;
begin
  tcaejlbn : entity work.yjscueu
    port map (ty => ranoi, c => ptwwowc);
  zbemb : entity work.t
    port map (malp => ftlppmat, vorawr => myoshxpnp, atry => vje);
  cgnt : entity work.fz
    port map (piryvyfdvy => qear, lylwegkq => yfd);
  kmoqrrtipx : entity work.t
    port map (malp => ftlppmat, vorawr => fswgbvyyc, atry => odsqzht);
  
  -- Single-driven assignments
  ieisvj <= (16#B# us, 23344 ps, 8#3# ns);
  ftlppmat <= ftlppmat;
  e <= (others => TRUE);
end dsmepx;



-- Seed after: 10142062612375860018,5983430343285687595
