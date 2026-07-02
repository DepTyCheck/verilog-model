-- Seed: 2946978866521279091,14426950258250697445

use std.reflection.all;

entity gfdwwcjwwq is
  port (v : inout file_subtype_mirror; kyrofwvk : in time; kvbw : inout access_subtype_mirror);
end gfdwwcjwwq;

architecture qugd of gfdwwcjwwq is
  
begin
  
end qugd;

use std.reflection.all;

entity kbyyiwp is
  port (yzvuvu : inout enumeration_subtype_mirror; igxafh : inout file_subtype_mirror);
end kbyyiwp;

use std.reflection.all;

architecture d of kbyyiwp is
  shared variable a : access_subtype_mirror;
  shared variable t : file_subtype_mirror;
  shared variable xasjovlviz : access_subtype_mirror;
  signal bti : time;
begin
  xbzfpysw : entity work.gfdwwcjwwq
    port map (v => igxafh, kyrofwvk => bti, kvbw => xasjovlviz);
  ji : entity work.gfdwwcjwwq
    port map (v => t, kyrofwvk => bti, kvbw => a);
end d;

use std.reflection.all;

entity owprabyg is
  port (ffldwt : inout protected_value_mirror);
end owprabyg;

use std.reflection.all;

architecture a of owprabyg is
  shared variable ijkumcrn : access_subtype_mirror;
  shared variable oqtbnxq : file_subtype_mirror;
  shared variable vzzqibleh : access_subtype_mirror;
  signal wom : time;
  shared variable qsmtgxofsg : file_subtype_mirror;
  shared variable uxruas : file_subtype_mirror;
  shared variable pddwzriblo : enumeration_subtype_mirror;
begin
  vk : entity work.kbyyiwp
    port map (yzvuvu => pddwzriblo, igxafh => uxruas);
  lqrjqp : entity work.gfdwwcjwwq
    port map (v => qsmtgxofsg, kyrofwvk => wom, kvbw => vzzqibleh);
  wkcktyuy : entity work.gfdwwcjwwq
    port map (v => oqtbnxq, kyrofwvk => wom, kvbw => ijkumcrn);
  
  -- Single-driven assignments
  wom <= wom;
end a;



-- Seed after: 18215964142452219546,14426950258250697445
