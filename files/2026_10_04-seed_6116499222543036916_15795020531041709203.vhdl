-- Seed: 6116499222543036916,15795020531041709203

entity xjjsj is
  port (axxgvjaqqh : buffer time_vector(0 to 4));
end xjjsj;

architecture qxaxuikwd of xjjsj is
  
begin
  -- Single-driven assignments
  axxgvjaqqh <= (8#14# ps, 44220.4431 ps, 1 ms, 3343 fs, 0 min);
end qxaxuikwd;

entity slzdgndkvx is
  port (l : out time; ctmbz : buffer time; yibdl : inout time);
end slzdgndkvx;

architecture lmxwlaylq of slzdgndkvx is
  signal exrpwkvk : time_vector(0 to 4);
begin
  qzadtzv : entity work.xjjsj
    port map (axxgvjaqqh => exrpwkvk);
  
  -- Single-driven assignments
  yibdl <= ctmbz;
end lmxwlaylq;



-- Seed after: 12102726816179408624,15795020531041709203
