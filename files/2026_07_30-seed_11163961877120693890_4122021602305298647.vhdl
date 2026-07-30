-- Seed: 11163961877120693890,4122021602305298647

entity xup is
  port (vkrpsotzxt : out boolean; bzbjuxkhjc : inout time);
end xup;

architecture ubhevvp of xup is
  
begin
  -- Single-driven assignments
  bzbjuxkhjc <= 16#F_C_2_1_D.D_3_F# ns;
  vkrpsotzxt <= FALSE;
end ubhevvp;

entity xlinem is
  port (tmphlrm : out real; dsn : inout boolean);
end xlinem;

architecture nngqmebqa of xlinem is
  signal jdgxvfzwhy : time;
  signal uvai : boolean;
  signal scd : time;
begin
  ktkdt : entity work.xup
    port map (vkrpsotzxt => dsn, bzbjuxkhjc => scd);
  t : entity work.xup
    port map (vkrpsotzxt => uvai, bzbjuxkhjc => jdgxvfzwhy);
end nngqmebqa;



-- Seed after: 7912244811846682439,4122021602305298647
