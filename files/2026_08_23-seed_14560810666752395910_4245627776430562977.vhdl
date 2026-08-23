-- Seed: 14560810666752395910,4245627776430562977

entity owmuqun is
  port (akqqm : buffer time_vector(3 downto 0); xfnqpncrbs : linkage boolean_vector(1 to 2));
end owmuqun;

architecture h of owmuqun is
  
begin
  -- Single-driven assignments
  akqqm <= (4 sec, 1 hr, 43.40134 ms, 0_1.4 fs);
end h;

entity wfphhagz is
  port (kyxmizcyn : in bit; zbfzc : in time);
end wfphhagz;

architecture mmtt of wfphhagz is
  signal rk : boolean_vector(1 to 2);
  signal adtcejpb : time_vector(3 downto 0);
  signal pegch : boolean_vector(1 to 2);
  signal qwqowajz : time_vector(3 downto 0);
  signal ei : boolean_vector(1 to 2);
  signal z : time_vector(3 downto 0);
begin
  e : entity work.owmuqun
    port map (akqqm => z, xfnqpncrbs => ei);
  pwishdnnie : entity work.owmuqun
    port map (akqqm => qwqowajz, xfnqpncrbs => pegch);
  xxpfs : entity work.owmuqun
    port map (akqqm => adtcejpb, xfnqpncrbs => rk);
end mmtt;



-- Seed after: 1495463604799641496,4245627776430562977
