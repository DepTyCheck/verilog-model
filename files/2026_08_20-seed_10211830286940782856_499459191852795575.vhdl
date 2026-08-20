-- Seed: 10211830286940782856,499459191852795575

entity t is
  port (bggb : out time; nbgz : linkage time_vector(1 to 3));
end t;

architecture swsif of t is
  
begin
  -- Single-driven assignments
  bggb <= bggb;
end swsif;

entity cqnhqnpn is
  port (b : in time; ljs : buffer time; yeoyyikji : out bit; reczxkf : in string(5 to 1));
end cqnhqnpn;

architecture zmqjnfzc of cqnhqnpn is
  signal fc : time_vector(1 to 3);
  signal amjh : time;
  signal ajrb : time_vector(1 to 3);
begin
  cwro : entity work.t
    port map (bggb => ljs, nbgz => ajrb);
  s : entity work.t
    port map (bggb => amjh, nbgz => fc);
  
  -- Single-driven assignments
  yeoyyikji <= '1';
end zmqjnfzc;

entity hblun is
  port (wtt : buffer time);
end hblun;

architecture wpqdai of hblun is
  signal zulgdo : time_vector(1 to 3);
begin
  lszv : entity work.t
    port map (bggb => wtt, nbgz => zulgdo);
end wpqdai;



-- Seed after: 15266763701600302404,499459191852795575
